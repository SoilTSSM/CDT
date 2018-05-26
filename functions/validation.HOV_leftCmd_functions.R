
Validation.HOV.PanelCmd <- function(clim.var){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(28)
		hscrlwin <- h.scale(45.5)
		wttkcombo <- as.integer(w.scale(30)/sfont0)
		largeur <- as.integer(w.scale(28)/sfont0)
		largeur1 <- as.integer(w.scale(30)/sfont0)
		largeur2 <- 30
		largeur3 <- 28
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(47)
		wttkcombo <- as.integer(w.scale(26)/sfont0)
		largeur <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(23)/sfont0)
		largeur2 <- 22
		largeur3 <- 20
	}

	GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'Validation_HOV.json'))
	MOIS <- format(ISOdate(2014, 1:12, 1), "%b")

	CHXSTATS0 <- c('Correlation', 'Nash-Sutcliffe Efficiency', 'Bias', 'Mean Absolute Error', 'Mean Error', 'Root Mean Square Error')
	CHXSTATS1 <- c('Probability Of Detection', 'False Alarm Ratio', 'Frequency Bias', 'Critical Success Index', 'Heidke Skill Score')
	CHXSTATS2 <- c('Volumetric Hit Index', 'Quantile Probability of Detection', 'Volumetric False Alarm Ratio',
					'Quantile False Alarm Ratio', 'Volumetric Miss Index', 'Volumetric Critical Success Index',
					'Quantile Critical Success Index')
	CHXSTATS <- c(CHXSTATS0, CHXSTATS1, CHXSTATS2)

	##############
	EnvZoomPars$xx1 <- tclVar()
	EnvZoomPars$xx2 <- tclVar()
	EnvZoomPars$yy1 <- tclVar()
	EnvZoomPars$yy2 <- tclVar()

	EnvZoomPars$pressButP <- tclVar(0)
	EnvZoomPars$pressButM <- tclVar(0)
	EnvZoomPars$pressButRect <- tclVar(0)
	EnvZoomPars$pressButDrag <- tclVar(0)

	EnvHOValidationplot$pressGetCoords <- tclVar(0)

	ZoomXYval0 <- NULL

	notebookTab <- NULL

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Extraction")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Validation")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Plot")
	cmd.tab5 <- bwAddTab(tknote.cmd, text = "Boundaries&DEM")

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

	##############################################

		frInputData <- ttklabelframe(subfr1, text = "Input data", relief = 'groove')

		file.period <- tclVar()
		CbperiodVAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data')
		tclvalue(file.period) <- switch(GeneralParameters$Tstep, 
										'daily' = CbperiodVAL[1], 
										'pentad' = cb.periodVAL[2],
										'dekadal' = CbperiodVAL[3],
										'monthly' = CbperiodVAL[4])
		file.stnfl <- tclVar(GeneralParameters$STN.file)
		dirNetCDF <- tclVar(GeneralParameters$ncdf.file$dir)

		cb.tstep <- ttkcombobox(frInputData, values = CbperiodVAL, textvariable = file.period)
		txt.stnfl <- tklabel(frInputData, text = 'Station data file', anchor = 'w', justify = 'left')
		cb.stnfl <- ttkcombobox(frInputData, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur)
		bt.stnfl <- tkbutton(frInputData, text = "...")

		txt.dir.ncdf <- tklabel(frInputData, text = "Directory of NetCDF files", anchor = 'w', justify = 'left')
		set.dir.ncdf <- tkbutton(frInputData, text = "Settings")
		en.dir.ncdf <- tkentry(frInputData, textvariable = dirNetCDF, width = largeur1)
		bt.dir.ncdf <- tkbutton(frInputData, text = "...")

		#######################

		tkconfigure(bt.stnfl, command = function(){
			dat.opfiles <- getOpenFiles(main.win, all.opfiles)
			if(!is.null(dat.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'ascii'
				AllOpenFilesData[[nopf+1]] <<- dat.opfiles
				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]

				tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
				tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.dispShp)
				tkconfigure(cb.adddem, values = unlist(listOpenFiles), textvariable = file.grddem)
				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)
			}else return(NULL)
		})

		tkconfigure(set.dir.ncdf, command = function(){
			GeneralParameters[["ncdf.file"]] <<- getInfoNetcdfData(main.win, GeneralParameters[["ncdf.file"]], str_trim(tclvalue(dirNetCDF)), tclvalue(file.period))
		})

		tkconfigure(bt.dir.ncdf, command = function(){
			dirnc <- tk_choose.dir(getwd(), "")
			tclvalue(dirNetCDF) <- if(!is.na(dirnc)) dirnc else ""
		})

		#######################

		tkgrid(cb.tstep, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(txt.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.stnfl, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		tkgrid(txt.dir.ncdf, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(set.dir.ncdf, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.dir.ncdf, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.ncdf, row = 4, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(cb.tstep, 'Select the time step of the data')
		status.bar.display(cb.tstep, TextOutputVar, 'Select the time step of the data')
		infobulle(cb.stnfl, 'Select the station data from the list')
		status.bar.display(cb.stnfl, TextOutputVar, 'Select the file containing the station data in CDT format')
		infobulle(bt.stnfl, 'Browse file if not listed')
		status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')

		infobulle(en.dir.ncdf, 'Enter the full path to the directory containing the NetCDF files')
		status.bar.display(en.dir.ncdf, TextOutputVar, 'Enter the full path to the directory containing the NetCDF files')
		infobulle(bt.dir.ncdf, 'Or browse here')
		status.bar.display(bt.dir.ncdf, TextOutputVar, 'Or browse here')
		infobulle(set.dir.ncdf, 'Setting netcdf data options')
		status.bar.display(set.dir.ncdf, TextOutputVar, 'Setting netcdf data options')

		#######################

		tkbind(cb.tstep, "<<ComboboxSelected>>", function(){
			tclvalue(day.txtVar) <- switch(tclvalue(file.period), 'Dekadal data' = 'Dek', 'Pentad data' = 'Pen', 'Day')
			statedate <- if(tclvalue(file.period) == 'Monthly data') 'disabled' else 'normal'
			tkconfigure(en.day1, state = statedate)
			tkconfigure(en.day2, state = statedate)

			AGGREGFUN <- c("mean", "sum", "count")
			if(tclvalue(aggr.data) == "0"){
				stateo0a <- "disabled"
				stateo0b <- "disabled"
				stateo1 <- "disabled"
				stateo2 <- "disabled"
			}else{
				if(tclvalue(file.period) != 'Daily data'){
					AGGREGFUN <- AGGREGFUN[-3]
					tclvalue(aggr.fun) <- if(tclvalue(aggr.fun) == "count") "sum" else tclvalue(aggr.fun)
				}
				stateo0a <- "readonly"
				stateo0b <- "normal"
				stateo1 <- if(tclvalue(aggr.fun) == "count") "readonly" else "disabled"
				stateo2 <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
			}

			tkconfigure(cb.aggfun, values = AGGREGFUN, state = stateo0a)
			# tkconfigure(en.minfrac, state = stateo0b)
			tkconfigure(cb.opfun, state = stateo1)
			tkconfigure(en.opthres, state = stateo2)
			tkconfigure(cb.stats.maps, values = CHXSTATS)
		})

		##############################################

		frtxtDate <- ttklabelframe(subfr1, text = "Extraction Date Range", relief = 'groove')

		istart.yrs <- tclVar(GeneralParameters$Extract.Date$start.year)
		istart.mon <- tclVar(GeneralParameters$Extract.Date$start.mon)
		istart.day <- tclVar(GeneralParameters$Extract.Date$start.dek)
		iend.yrs <- tclVar(GeneralParameters$Extract.Date$end.year)
		iend.mon <- tclVar(GeneralParameters$Extract.Date$end.mon)
		iend.day <- tclVar(GeneralParameters$Extract.Date$end.dek)

		txtdek <- switch(GeneralParameters$Tstep, 'dekadal' = 'Dek', 'pentad' = 'Pen', 'Day')
		day.txtVar <- tclVar(txtdek)
		statedate <- if(GeneralParameters$Tstep == 'monthly') 'disabled' else 'normal'

		txt.deb <- tklabel(frtxtDate, text = 'Start date', anchor = 'e', justify = 'right')
		txt.fin <- tklabel(frtxtDate, text = 'End date', anchor = 'e', justify = 'right')
		txt.yrs <- tklabel(frtxtDate, text = 'Year')
		txt.mon <- tklabel(frtxtDate, text = 'Month')
		txt.day <- tklabel(frtxtDate, text = tclvalue(day.txtVar), textvariable = day.txtVar)
		en.yrs1 <- tkentry(frtxtDate, width = 4, textvariable = istart.yrs, justify = "right")
		en.mon1 <- tkentry(frtxtDate, width = 4, textvariable = istart.mon, justify = "right")
		en.day1 <- tkentry(frtxtDate, width = 4, textvariable = istart.day, justify = "right", state = statedate)
		en.yrs2 <- tkentry(frtxtDate, width = 4, textvariable = iend.yrs, justify = "right")
		en.mon2 <- tkentry(frtxtDate, width = 4, textvariable = iend.mon, justify = "right")
		en.day2 <- tkentry(frtxtDate, width = 4, textvariable = iend.day, justify = "right", state = statedate)

		tkgrid(txt.deb, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.fin, row = 2, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.yrs, row = 0, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.mon, row = 0, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.day, row = 0, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.yrs1, row = 1, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.mon1, row = 1, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.day1, row = 1, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.yrs2, row = 2, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.mon2, row = 2, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.day2, row = 2, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(frtxtDate, 'Start and end date to perform the merging')
		status.bar.display(frtxtDate, TextOutputVar, 'Start and end date to perform the merging')

		##############################################

		frameDirSav <- ttklabelframe(subfr1, text = "Directory to save result", relief = 'groove')

		file.save1 <- tclVar(GeneralParameters$outdir)

		en.dir.save <- tkentry(frameDirSav, textvariable = file.save1, width = largeur1)
		bt.dir.save <- tkbutton(frameDirSav, text = "...")
		#######################

		tkconfigure(bt.dir.save, command = function() fileORdir2Save(file.save1, isFile = FALSE))

		#############################

		tkgrid(en.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.save, row = 0, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.dir.save, 'Enter the full path to the directory  to save result')
		status.bar.display(en.dir.save, TextOutputVar, 'Enter the full path to the directory to save result')
		infobulle(bt.dir.save, 'Browse here the full path to the directory to save result')
		status.bar.display(bt.dir.save, TextOutputVar, 'Browse here the full path to the directory to save result')

		#############################
		tkgrid(frInputData, row = 0, column = 0, sticky = 'we')
		tkgrid(frtxtDate, row = 1, column = 0, sticky = '', pady = 1)
		tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', pady = 3)

	#######################################################################################################

	#Tab2
	frTab2 <- tkframe(cmd.tab2)
	tkgrid(frTab2, padx = 5, pady = 5, ipadx = 2, ipady = 2)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)
	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr2, 0, weight = 1)

	##############################################

		frameSelect <- ttklabelframe(subfr2, text = "Selection Type", relief = 'groove')

		EnvHOValidationplot$type.select <- tclVar()
		SELECTALL <- c('All Stations', 'Rectangle', 'Polygons')
		tclvalue(EnvHOValidationplot$type.select) <- switch(GeneralParameters$type.select, 
														'all' = SELECTALL[1], 
														'rect' = SELECTALL[2],
														'poly' = SELECTALL[3])

		cb.type.select <- ttkcombobox(frameSelect, values = SELECTALL, textvariable = EnvHOValidationplot$type.select, width = wttkcombo)

		tkgrid(cb.type.select, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

		######
		tkbind(cb.type.select, "<<ComboboxSelected>>", function(){
			selectedPolygon <- NULL

			if(tclvalue(EnvHOValidationplot$type.select) == 'All Stations'){
				statelonlat <- 'disabled'
				statepolygon <- 'disabled'
			}

			if(tclvalue(EnvHOValidationplot$type.select) == 'Rectangle'){
				statelonlat <- 'normal'
				statepolygon <- 'disabled'
			}

			if(tclvalue(EnvHOValidationplot$type.select) == 'Polygons'){
				statelonlat <- 'disabled'
				statepolygon <- 'normal'

				if(tclvalue(EnvHOValidationplot$namePoly) != ''){
					shpfopen <- getShpOpenData(file.dispShp)
					if(!is.null(shpfopen)){
						shpf <- shpfopen[[2]]
						ids <- as.numeric(tclvalue(tcl(EnvHOValidationplot$cb.shpAttr, 'current')))+1
						selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(EnvHOValidationplot$namePoly), ])
					}
				}
			}

			tkconfigure(en.minlon, state = statelonlat)
			tkconfigure(en.maxlon, state = statelonlat)
			tkconfigure(en.minlat, state = statelonlat)
			tkconfigure(en.maxlat, state = statelonlat)
			tkconfigure(EnvHOValidationplot$cb.shpAttr, state = statepolygon)
			tkconfigure(cb.Polygon, state = statepolygon)

			##
			tclvalue(EnvHOValidationplot$minlonRect) <- ''
			tclvalue(EnvHOValidationplot$maxlonRect) <- ''
			tclvalue(EnvHOValidationplot$minlatRect) <- ''
			tclvalue(EnvHOValidationplot$maxlatRect) <- ''
			tkconfigure(EnvHOValidationplot$bt.select, relief = 'raised', bg = 'lightblue', state = 'normal')

			tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))+1
			if(length(AllOpenTabType) > 0){
				if(AllOpenTabType[[tabid]] == "img" & !is.null(notebookTab)){
					if(AllOpenTabData[[tabid]][[1]][[1]]$ID  == notebookTab[[2]]){
						assign("selectedPolygon", selectedPolygon, envir = environment(AllOpenTabData[[tabid]][[2]][[2]]$fun))
						refreshPlot1(W = AllOpenTabData[[tabid]][[2]][[1]],
									img = AllOpenTabData[[tabid]][[2]][[2]],
									hscale = as.numeric(tclvalue(tkget(spinH))),
									vscale = as.numeric(tclvalue(tkget(spinV))))
						tkdelete(tkwinfo('children', AllOpenTabData[[tabid]][[1]][[2]]), 'rect')
					}
				}
			}
		})

		##############################################

		frameShp <- ttklabelframe(subfr2, text = "Boundaries Shapefiles", relief = 'groove')

		file.dispShp <- tclVar(GeneralParameters$shp.file$shp)
		shpAttr <- tclVar(GeneralParameters$shp.file$attr)
		EnvHOValidationplot$namePoly <- tclVar()

		cb.shpF <- ttkcombobox(frameShp, values = unlist(listOpenFiles), textvariable = file.dispShp, width = largeur)
		bt.shpF <- tkbutton(frameShp, text = "...")
		txt.attr.shpF <- tklabel(frameShp, text = "Attribute field to be used and displayed", anchor = 'w', justify = 'left')
		EnvHOValidationplot$cb.shpAttr <- ttkcombobox(frameShp, values='', textvariable = shpAttr, width = wttkcombo, state = 'disabled')
		cb.Polygon <- ttkcombobox(frameShp, values = '', textvariable = EnvHOValidationplot$namePoly, width = wttkcombo, state = 'disabled')

		tkgrid(cb.shpF, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.shpF, row = 0, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)
		tkgrid(txt.attr.shpF, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
		tkgrid(EnvHOValidationplot$cb.shpAttr, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 2)
		tkgrid(cb.Polygon, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 2)

		#######################
		tkconfigure(bt.shpF, command = function(){
			shp.opfiles <- getOpenShp(main.win, all.opfiles)
			if(!is.null(shp.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'shp'
				AllOpenFilesData[[nopf+1]] <<- shp.opfiles
				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(file.dispShp) <- AllOpenFilesData[[nopf+1]][[1]]

				tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
				tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.dispShp)
				tkconfigure(cb.adddem, values = unlist(listOpenFiles), textvariable = file.grddem)
				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)

				###
				shpf <- getShpOpenData(file.dispShp)
				dat <- shpf[[2]]@data
				AttrTable <- names(dat)
				tclvalue(shpAttr) <- AttrTable[1]

				adminN <- as.character(dat[, 1])
				name.poly <- levels(as.factor(adminN))
				if(length(name.poly) < 2) name.poly <- c(name.poly, "")
				tclvalue(EnvHOValidationplot$namePoly) <- name.poly[1]

				tkconfigure(EnvHOValidationplot$cb.shpAttr, values = AttrTable, textvariable = shpAttr)
				tkconfigure(cb.Polygon, values = name.poly, textvariable = EnvHOValidationplot$namePoly)
			}
		})

		#######################
		tkbind(cb.shpF, "<<ComboboxSelected>>", function(){
			shpf <- getShpOpenData(file.dispShp)
			if(!is.null(shpf)){
				dat <- shpf[[2]]@data
				AttrTable <- names(dat)
				tclvalue(shpAttr) <- AttrTable[1]
				adminN <- as.character(dat[, as.numeric(tclvalue(tcl(EnvHOValidationplot$cb.shpAttr, 'current')))+1])
				name.poly <- levels(as.factor(adminN))
				if(length(name.poly) < 2) name.poly <- c(name.poly, "")
			}else{
				AttrTable <- ''
				tclvalue(shpAttr) <- ''
				name.poly <- ''
				tclvalue(EnvHOValidationplot$namePoly) <- ''
			}
			tkconfigure(EnvHOValidationplot$cb.shpAttr, values = AttrTable, textvariable = shpAttr)
			tkconfigure(cb.Polygon, values = name.poly, textvariable = EnvHOValidationplot$namePoly)
		})

		########################
		tkbind(EnvHOValidationplot$cb.shpAttr, "<<ComboboxSelected>>", function(){
			shpf <- getShpOpenData(file.dispShp)
			if(!is.null(shpf)){
				dat <- shpf[[2]]@data
				adminN <- as.character(dat[, as.numeric(tclvalue(tcl(EnvHOValidationplot$cb.shpAttr, 'current')))+1])
				name.poly <- levels(as.factor(adminN))
				if(length(name.poly) < 2) name.poly <- c(name.poly, "")
			}else{
				name.poly <- ''
			}
			tclvalue(EnvHOValidationplot$namePoly) <- name.poly[1]
			tkconfigure(cb.Polygon, values = name.poly, textvariable = EnvHOValidationplot$namePoly)
		})

		########################
		tkbind(cb.Polygon, "<<ComboboxSelected>>", function(){
			if(tclvalue(EnvHOValidationplot$namePoly) != ''){
				shpfopen <- getShpOpenData(file.dispShp)
				if(!is.null(shpfopen)){
					shpf <- shpfopen[[2]]
					ids <- as.numeric(tclvalue(tcl(EnvHOValidationplot$cb.shpAttr, 'current')))+1
					selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(EnvHOValidationplot$namePoly), ])
				}else selectedPolygon <- NULL
			}else{
				selectedPolygon <- NULL
			}

			tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))+1
			if(length(AllOpenTabType) > 0){
				if(AllOpenTabType[[tabid]] == "img" & !is.null(notebookTab)){
					if(AllOpenTabData[[tabid]][[1]][[1]]$ID  == notebookTab[[2]]){
						assign("selectedPolygon", selectedPolygon, envir = environment(AllOpenTabData[[tabid]][[2]][[2]]$fun))
						refreshPlot1(W = AllOpenTabData[[tabid]][[2]][[1]],
									img = AllOpenTabData[[tabid]][[2]][[2]],
									hscale = as.numeric(tclvalue(tkget(spinH))),
									vscale = as.numeric(tclvalue(tkget(spinV))))
					}
				}
			}
		})

		##############################################

		frameIMgMan <- tkframe(subfr2)

		#######################

		frameZoom <- ttklabelframe(frameIMgMan, text = "ZOOM", relief = 'groove')

		EnvZoomPars$btZoomP <- tkbutton(frameZoom, image = pikZoomPlus, relief = 'raised', bg = 'lightblue', state = 'normal')
		EnvZoomPars$btZoomM <- tkbutton(frameZoom, image = pikZoomMinus, relief = 'raised', bg = 'lightblue', state = 'normal')
		EnvZoomPars$btZoomRect <- tkbutton(frameZoom, image = pikZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
		EnvZoomPars$btPanImg <- tkbutton(frameZoom, image = pikPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
		EnvZoomPars$btRedraw <- tkbutton(frameZoom, image = pikRedraw, relief = 'raised', state = 'disabled')
		EnvZoomPars$btReset <- tkbutton(frameZoom, image = pikReset, relief = 'raised')

		#######################
		tkgrid(EnvZoomPars$btZoomP, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(EnvZoomPars$btZoomM, row = 0, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(EnvZoomPars$btZoomRect, row = 0, column = 2, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(EnvZoomPars$btReset, row = 1, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(EnvZoomPars$btRedraw, row = 1, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(EnvZoomPars$btPanImg, row = 1, column = 2, sticky = 'nswe', rowspan = 1, columnspan = 1)

		infobulle(EnvZoomPars$btZoomP, 'Zoom In')
		status.bar.display(EnvZoomPars$btZoomP, TextOutputVar, 'Zoom In')
		infobulle(EnvZoomPars$btZoomM, 'Zoom Out')
		status.bar.display(EnvZoomPars$btZoomM, TextOutputVar, 'Zoom Out')
		infobulle(EnvZoomPars$btZoomRect, 'Zoom Area')
		status.bar.display(EnvZoomPars$btZoomRect, TextOutputVar, 'Zoom Area')
		infobulle(EnvZoomPars$btPanImg, 'Pan Tool')
		status.bar.display(EnvZoomPars$btPanImg, TextOutputVar, 'Pan Tool')
		infobulle(EnvZoomPars$btRedraw, 'Redraw Map')
		status.bar.display(EnvZoomPars$btRedraw, TextOutputVar, 'Redraw Map')
		infobulle(EnvZoomPars$btReset,' Zoom Reset')
		status.bar.display(EnvZoomPars$btReset, TextOutputVar,' Zoom Reset')

		##############################################

		frameDisp <- tkframe(frameIMgMan)

		EnvHOValidationplot$minlonRect <- tclVar()
		EnvHOValidationplot$maxlonRect <- tclVar()
		EnvHOValidationplot$minlatRect <- tclVar()
		EnvHOValidationplot$maxlatRect <- tclVar()

		bt.dispMap <- tkbutton(frameDisp, text = "Display Map")
		EnvHOValidationplot$bt.select <- tkbutton(frameDisp, text = "Select", relief = 'raised', bg = 'lightblue')

		txt.minLab <- tklabel(frameDisp, text = 'Min')
		txt.maxLab <- tklabel(frameDisp, text = 'Max')
		txt.lonLab <- tklabel(frameDisp, text = 'Lon', anchor = 'e', justify = 'right')
		txt.latLab <- tklabel(frameDisp, text = 'Lat', anchor = 'e', justify = 'right')
		en.minlon <- tkentry(frameDisp, width = 4, textvariable = EnvHOValidationplot$minlonRect, justify = "left", state = 'disabled')
		en.maxlon <- tkentry(frameDisp, width = 4, textvariable = EnvHOValidationplot$maxlonRect, justify = "left", state = 'disabled')
		en.minlat <- tkentry(frameDisp, width = 4, textvariable = EnvHOValidationplot$minlatRect, justify = "left", state = 'disabled')
		en.maxlat <- tkentry(frameDisp, width = 4, textvariable = EnvHOValidationplot$maxlatRect, justify = "left", state = 'disabled')

		#######################

		tkconfigure(bt.dispMap, command = function(){
			donne <- getStnOpenData(file.stnfl)
			shpofile <- getShpOpenData(file.dispShp)
			if(!is.null(donne)){
				EnvHOValidation$donne <- donne[1:3, -1]
				lonStn <- as.numeric(EnvHOValidation$donne[2, ])
				latStn <- as.numeric(EnvHOValidation$donne[3, ])
				lo1 <- min(lonStn, na.rm = TRUE)
				lo2 <- max(lonStn, na.rm = TRUE)
				la1 <- min(latStn, na.rm = TRUE)
				la2 <- max(latStn, na.rm = TRUE)
				plotOK <- TRUE
				shpf <- shpofile[[2]]
			}else{
				plotOK <- FALSE
				InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
			}

			if(tclvalue(EnvHOValidationplot$type.select) == 'Polygons' & plotOK){
				if(!is.null(shpofile)){
					shpf <- shpofile[[2]]
					bbxshp <- round(bbox(shpf), 4)
					lo1 <- min(lo1, bbxshp[1, 1])
					lo2 <- max(lo2, bbxshp[1, 2])
					la1 <- min(la1, bbxshp[2, 1])
					la2 <- max(la2, bbxshp[2, 2])
					plotOK <- TRUE
				}else{
					plotOK <- FALSE
					InsertMessagesTxt(main.txt.out, 'No ESRI shapfile for administrative boundaries found', format = TRUE)
				}
			}

			if(plotOK){
				ZoomXYval0 <<- c(lo1, lo2, la1, la2)
				tclvalue(EnvZoomPars$xx1) <- lo1
				tclvalue(EnvZoomPars$xx2) <- lo2
				tclvalue(EnvZoomPars$yy1) <- la1
				tclvalue(EnvZoomPars$yy2) <- la2
				ZoomXYval <- as.numeric(c(tclvalue(EnvZoomPars$xx1), tclvalue(EnvZoomPars$xx2),
										tclvalue(EnvZoomPars$yy1), tclvalue(EnvZoomPars$yy2)))

				imgContainer <- displayMap4Validation(tknotes, shpf, ZoomXYval, notebookTab)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab, AllOpenTabType, AllOpenTabData)
				notebookTab <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		#######################

		tkgrid(bt.dispMap, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
		tkgrid(EnvHOValidationplot$bt.select, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(txt.minLab, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(txt.maxLab, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(txt.lonLab, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(txt.latLab, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(en.minlon, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(en.maxlon, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(en.minlat, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(en.maxlat, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)

		#######################
		tkgrid(frameZoom, row = 0, column = 0, sticky = 'ns', columnspan = 2, ipady = 5)
		tkgrid(frameDisp, row = 0, column = 2, sticky = 'we', columnspan = 4)

		##############################################

		if(!is.null(EnvHOValidationplot$hovd)){
			stateBTEx <- if(tclvalue(EnvHOValidationplot$hovd) == "1") "normal" else "disabled"
		}else stateBTEx <- "normal"

		bt.extract.station <- ttkbutton(subfr2, text = "Extract Data for Validation", state = stateBTEx)

		tkconfigure(bt.extract.station, command = function(){
			GeneralParameters$clim.var <- clim.var
			GeneralParameters$Tstep <- switch(tclvalue(file.period),
			 								'Daily data' = 'daily',
			 								'Pentad data' = 'pentad',
											'Dekadal data' =  'dekadal',
											'Monthly data' = 'monthly')
			GeneralParameters$STN.file <- str_trim(tclvalue(file.stnfl))
			GeneralParameters$ncdf.file$dir <- str_trim(tclvalue(dirNetCDF))
			GeneralParameters$outdir <- str_trim(tclvalue(file.save1))

			GeneralParameters$Extract.Date$start.year <- as.numeric(str_trim(tclvalue(istart.yrs)))
			GeneralParameters$Extract.Date$start.mon <- as.numeric(str_trim(tclvalue(istart.mon)))
			GeneralParameters$Extract.Date$start.dek <- as.numeric(str_trim(tclvalue(istart.day)))
			GeneralParameters$Extract.Date$end.year <- as.numeric(str_trim(tclvalue(iend.yrs)))
			GeneralParameters$Extract.Date$end.mon <- as.numeric(str_trim(tclvalue(iend.mon)))
			GeneralParameters$Extract.Date$end.dek <- as.numeric(str_trim(tclvalue(iend.day)))

			GeneralParameters$shp.file$shp <- str_trim(tclvalue(file.dispShp))
			GeneralParameters$shp.file$attr <- str_trim(tclvalue(shpAttr))

			GeneralParameters$type.select <- switch(tclvalue(EnvHOValidationplot$type.select),
														'All Stations' = 'all',
														'Rectangle' = 'rect',
														'Polygons' = 'poly')
			GeneralParameters$Geom <- NULL
			GeneralParameters$Geom$minlon <- as.numeric(str_trim(tclvalue(EnvHOValidationplot$minlonRect)))
			GeneralParameters$Geom$maxlon <- as.numeric(str_trim(tclvalue(EnvHOValidationplot$maxlonRect)))
			GeneralParameters$Geom$minlat <- as.numeric(str_trim(tclvalue(EnvHOValidationplot$minlatRect)))
			GeneralParameters$Geom$maxlat <- as.numeric(str_trim(tclvalue(EnvHOValidationplot$maxlatRect)))
			GeneralParameters$Geom$namePoly <- str_trim(tclvalue(EnvHOValidationplot$namePoly))

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			InsertMessagesTxt(main.txt.out, "Extract data .................")
			tcl('update')
			ret <- tryCatch(
				HOV_DataExtraction(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e){
					 errorFun(e)
				},
				finally = {
					tkconfigure(main.win, cursor = '')
				}
			)

			msg0 <- "Data extraction finished successfully"
			msg1 <- "Data extraction failed"
			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)
				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})

		#######################

		tkgrid(frameSelect, row = 0, column = 0, sticky = 'we')
		tkgrid(frameShp, row = 1, column = 0, sticky = 'we', pady = 3)
		tkgrid(frameIMgMan, row = 2, column = 0, sticky = 'we', pady = 3)
		tkgrid(bt.extract.station, row = 3, column = 0, sticky = 'we', pady = 3)

		##############################################

		tkconfigure(EnvZoomPars$btReset, command = function(){
			ZoomXYval <<- ZoomXYval0
			tclvalue(EnvZoomPars$xx1) <- ZoomXYval0[1]
			tclvalue(EnvZoomPars$xx2) <- ZoomXYval0[2]
			tclvalue(EnvZoomPars$yy1) <- ZoomXYval0[3]
			tclvalue(EnvZoomPars$yy2) <- ZoomXYval0[4]
			
			tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))+1
			if(length(AllOpenTabType) > 0){
				if(AllOpenTabType[[tabid]] == "img" & !is.null(notebookTab)){
					if(AllOpenTabData[[tabid]][[1]][[1]]$ID  == notebookTab[[2]]){
						assign("ZoomXYval", ZoomXYval, envir = environment(AllOpenTabData[[tabid]][[2]][[2]]$fun))
						refreshPlot1(W = AllOpenTabData[[tabid]][[2]][[1]],
									img = AllOpenTabData[[tabid]][[2]][[2]],
									hscale = as.numeric(tclvalue(tkget(spinH))),
									vscale = as.numeric(tclvalue(tkget(spinV))))
					}
				}
			}
		})

		##########################

		tkbind(EnvZoomPars$btReset, "<Button-1>", function(){
			tclvalue(EnvZoomPars$pressButP) <- 0
			tclvalue(EnvZoomPars$pressButM) <- 0
			tclvalue(EnvZoomPars$pressButRect) <- 0
			tclvalue(EnvZoomPars$pressButDrag) <- 0

			tclvalue(EnvHOValidationplot$pressGetCoords) <- 0

			tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(EnvHOValidationplot$bt.select, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(EnvZoomPars$btZoomP, "<Button-1>", function(){
			tclvalue(EnvZoomPars$pressButP) <- 1
			tclvalue(EnvZoomPars$pressButM) <- 0
			tclvalue(EnvZoomPars$pressButRect) <- 0
			tclvalue(EnvZoomPars$pressButDrag) <- 0

			tclvalue(EnvHOValidationplot$pressGetCoords) <- 0

			tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'red', state = 'disabled')
			tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(EnvHOValidationplot$bt.select, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(EnvZoomPars$btZoomM, "<Button-1>", function(){
			tclvalue(EnvZoomPars$pressButP) <- 0
			tclvalue(EnvZoomPars$pressButM) <- 1
			tclvalue(EnvZoomPars$pressButRect) <- 0
			tclvalue(EnvZoomPars$pressButDrag) <- 0

			tclvalue(EnvHOValidationplot$pressGetCoords) <- 0

			tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'red', state = 'disabled')
			tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(EnvHOValidationplot$bt.select, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(EnvZoomPars$btZoomRect, "<Button-1>", function(){
			tclvalue(EnvZoomPars$pressButP) <- 0
			tclvalue(EnvZoomPars$pressButM) <- 0
			tclvalue(EnvZoomPars$pressButRect) <- 1
			tclvalue(EnvZoomPars$pressButDrag) <- 0

			tclvalue(EnvHOValidationplot$pressGetCoords) <- 0

			tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'red', state = 'disabled')
			tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(EnvHOValidationplot$bt.select, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(EnvZoomPars$btPanImg, "<Button-1>", function(){
			tclvalue(EnvZoomPars$pressButP) <- 0
			tclvalue(EnvZoomPars$pressButM) <- 0
			tclvalue(EnvZoomPars$pressButRect) <- 0
			tclvalue(EnvZoomPars$pressButDrag) <- 1

			tclvalue(EnvHOValidationplot$pressGetCoords) <- 0

			tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'red', state = 'disabled')

			tkconfigure(EnvHOValidationplot$bt.select, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(EnvHOValidationplot$bt.select, "<Button-1>", function(){
			tclvalue(EnvZoomPars$pressButP) <- 0
			tclvalue(EnvZoomPars$pressButM) <- 0
			tclvalue(EnvZoomPars$pressButRect) <- 0
			tclvalue(EnvZoomPars$pressButDrag) <- 0

			tclvalue(EnvHOValidationplot$pressGetCoords) <- 1

			tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(EnvHOValidationplot$bt.select, relief = 'raised', bg = 'red', state = 'disabled')
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

	##############################################

		frameHOV <- ttklabelframe(subfr3, text = "Hold-Out Validation data", relief = 'groove')

		EnvHOValidationplot$hovd <- tclVar(0)
		file.hovd <- tclVar()

		stateHOVd <- if(tclvalue(EnvHOValidationplot$hovd) == "1") "normal" else "disabled"

		chk.hovd <- tkcheckbutton(frameHOV, variable = EnvHOValidationplot$hovd, text = "Hold-Out Validation already performed", anchor = 'w', justify = 'left')
		en.hovd <- tkentry(frameHOV, textvariable = file.hovd, width = largeur1, state = stateHOVd)
		bt.hovd <- tkbutton(frameHOV, text = "...", state = stateHOVd)

		tkconfigure(bt.hovd, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.hovd <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			if(path.hovd == "") return(NULL)
			tclvalue(file.hovd) <- path.hovd

			if(file.exists(tclvalue(file.hovd))){
				hovd.data <- try(readRDS(tclvalue(file.hovd)), silent = TRUE)
				if(inherits(hovd.data, "try-error")){
					InsertMessagesTxt(main.txt.out, 'Unable to load Hold-Out Validation data', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', hovd.data[1]), format = TRUE)
					return(NULL)
				}
				EnvHOValidation <<- hovd.data
				EnvHOValidationplot$file.hovd <- tclvalue(file.hovd)

				tclvalue(file.period) <- switch(hovd.data$GeneralParameters$Tstep, 
												'daily' = CbperiodVAL[1], 
												'pentad' = cb.periodVAL[2],
												'dekadal' = CbperiodVAL[3],
												'monthly' = CbperiodVAL[4])

				AGGREGFUN <- c("mean", "sum", "count")
				if(tclvalue(aggr.data) == "1"){
					if(tclvalue(file.period) != 'Daily data'){
						AGGREGFUN <- AGGREGFUN[-3]
						tclvalue(aggr.fun) <- if(tclvalue(aggr.fun) == "count") "sum" else tclvalue(aggr.fun)
					}
					stateo0a <- "readonly"
				}else stateo0a <- "disabled"

				tkconfigure(cb.aggfun, values = AGGREGFUN, state = stateo0a)
			}
		})

		tkgrid(chk.hovd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.hovd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.hovd, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.hovd, "<Button-1>", function(){
			stateHOVd <- if(tclvalue(EnvHOValidationplot$hovd) == '1') 'disabled' else 'normal'
			tkconfigure(en.hovd, state = stateHOVd)
			tkconfigure(bt.hovd, state = stateHOVd)
			stateBTEx <- if(tclvalue(EnvHOValidationplot$hovd) == '1') 'normal' else 'disabled'
			tkconfigure(bt.extract.station, state = stateBTEx)
		})

		##############################################

		frameSeason <- ttklabelframe(subfr3, text = "Years & Season", relief = 'groove')

		mon1 <- as.numeric(str_trim(GeneralParameters$date.range$start.month))
		mon2 <- as.numeric(str_trim(GeneralParameters$date.range$end.month))
		start.mois <- tclVar(MOIS[mon1])
		end.mois <- tclVar(MOIS[mon2])
		start.year <- tclVar(GeneralParameters$date.range$start.year)
		end.year <- tclVar(GeneralParameters$date.range$end.year)

		fr.seas <- ttklabelframe(frameSeason, text = 'Season', relief = 'sunken', labelanchor = "n", borderwidth = 2)
		fr.year <- ttklabelframe(frameSeason, text = 'Years', relief = 'sunken', labelanchor = "n", borderwidth = 2)

		txt.to1 <- tklabel(fr.year, text = '-to-')
		en.years1 <- tkentry(fr.year, width = 5, textvariable = start.year, justify = 'right')
		en.years2 <- tkentry(fr.year, width = 5, textvariable = end.year, justify = 'right')

		txt.to2 <- tklabel(fr.seas, text = '-to-')
		cb.month1 <- ttkcombobox(fr.seas, values = MOIS, textvariable = start.mois, width = 4)
		cb.month2 <- ttkcombobox(fr.seas, values = MOIS, textvariable = end.mois, width = 4)

		tkgrid(en.years1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
		tkgrid(txt.to1, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
		tkgrid(en.years2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

		tkgrid(cb.month1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
		tkgrid(txt.to2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
		tkgrid(cb.month2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

		tkgrid(fr.seas, row = 0, column = 0, sticky = 'ns', rowspan = 1, columnspan = 1, padx = 3, pady = 1)
		tkgrid(fr.year, row = 0, column = 1, sticky = 'ns', rowspan = 1, columnspan = 1, padx = 3, pady = 1)

		infobulle(en.years1, 'Start year of the period to calculate the statistics')
		status.bar.display(en.years1, TextOutputVar, 'Start year of the period to calculate the statistics')
		infobulle(en.years2, 'End year of the period to calculate the statistics')
		status.bar.display(en.years2, TextOutputVar, 'End year of the period to calculate the statistics')
		infobulle(cb.month1, 'Start month of the period to calculate the statistics')
		status.bar.display(cb.month1, TextOutputVar, 'Start month of the period to calculate the statistics')
		infobulle(cb.month2, 'End month of the season to calculate the statistics')
		status.bar.display(cb.month2, TextOutputVar, 'End month of the season to calculate the statistics')

		##############################################

		frameAggr <- ttklabelframe(subfr3, text = "Data aggregation", relief = 'groove')

		aggr.data <- tclVar(GeneralParameters$aggr.series$aggr.data)
		aggr.fun <- tclVar(GeneralParameters$aggr.series$aggr.fun)
		# min.frac <- tclVar(GeneralParameters$aggr.series$min.frac)
		opr.fun <- tclVar(GeneralParameters$aggr.series$opr.fun)
		opr.thres <- tclVar(GeneralParameters$aggr.series$opr.thres)

		AGGREGFUN <- c("mean", "sum", "count")
		if(GeneralParameters$Tstep != 'daily' & !GeneralParameters$aggr.series$aggr.data) AGGREGFUN <- AGGREGFUN[-3]
		if(!GeneralParameters$aggr.series$aggr.data){
			stateo0a <- 'disabled'
			stateo0b <- 'disabled'
			stateo1 <- 'disabled'
			stateo2 <- 'disabled'
		}else{
			stateo0a <- 'readonly'
			stateo0b <- 'normal'
			stateo1 <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'readonly' else 'disabled'
			stateo2 <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'normal' else 'disabled'
		}

		chk.aggrdata <- tkcheckbutton(frameAggr, variable = aggr.data, text = "Aggregate data", anchor = 'w', justify = 'left')
		txt.aggfun <- tklabel(frameAggr, text = 'Function', anchor = 'w', justify = 'left')
		cb.aggfun <- ttkcombobox(frameAggr, values = AGGREGFUN, textvariable = aggr.fun, width = 6, state = stateo0a)
		# txt.minfrac <- tklabel(frameAggr, text = 'Min.Frac', anchor = 'w', justify = 'left')
		# en.minfrac <- tkentry(frameAggr, textvariable = min.frac, width = 6, state = stateo0b)
		txt.opfun <- tklabel(frameAggr, text = 'Operator', anchor = 'w', justify = 'left')
		cb.opfun <- ttkcombobox(frameAggr, values = c(">=", ">", "<=", "<"), textvariable = opr.fun, width = 6, state = stateo1)
		txt.opthres <- tklabel(frameAggr, text = 'Threshold', anchor = 'w', justify = 'left')
		en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = 6, state = stateo2)

		tkgrid(chk.aggrdata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.aggfun, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.aggfun, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		# tkgrid(txt.minfrac, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		# tkgrid(en.minfrac, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.opfun, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.opfun, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.opthres, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.opthres, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.aggfun, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
		status.bar.display(cb.aggfun, TextOutputVar, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
		# infobulle(en.minfrac, 'Minimum fraction of available data that must be present within each output time step')
		# status.bar.display(en.minfrac, TextOutputVar, 'Minimum fraction of available data that must be present within each output time step')
		infobulle(cb.opfun, 'Select the comparison operator to be used to match event')
		status.bar.display(cb.opfun, TextOutputVar, 'Select the comparison operator to be used to match event')
		infobulle(en.opthres, 'User defined threshold applied to count event')
		status.bar.display(en.opthres, TextOutputVar, 'User defined threshold applied to count event')

		#################
		tkbind(cb.aggfun, "<<ComboboxSelected>>", function(){
			stateo1 <- if(tclvalue(aggr.fun) == "count") "readonly" else "disabled"
			stateo2 <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
			tkconfigure(cb.opfun, state = stateo1)
			tkconfigure(en.opthres, state = stateo2)
		})

		tkbind(chk.aggrdata, "<Button-1>", function(){
			if(tclvalue(aggr.data) == "1"){
				stateo0a <- 'disabled'
				stateo0b <- 'disabled'
				stateo1 <- 'disabled'
				stateo2 <- 'disabled'
			}else{
				stateo0a <- 'readonly'
				stateo0b <- 'normal'
				stateo1 <- if(tclvalue(aggr.fun) == "count") 'readonly' else 'disabled'
				stateo2 <- if(tclvalue(aggr.fun) == "count") 'normal' else 'disabled'
			}

			tkconfigure(cb.aggfun, state = stateo0a)
			# tkconfigure(en.minfrac, state = stateo0b)
			tkconfigure(cb.opfun, state = stateo1)
			tkconfigure(en.opthres, state = stateo2)
			tkconfigure(cb.stats.maps, values = CHXSTATS)
		})

		#############################

		STATDATATYPE <- c('All Data', 'Spatial Average', 'Per station')
		stat.data <- tclVar()
		tclvalue(stat.data) <- switch(GeneralParameters$stat.data, 
										'all' = STATDATATYPE[1], 
										'avg' = STATDATATYPE[2],
										'stn' = STATDATATYPE[3])

		cb.stat.data <- ttkcombobox(subfr3, values = STATDATATYPE, textvariable = stat.data, width = wttkcombo)

		infobulle(cb.stat.data, 'Use all data or a spatial average or station by station to calculate the statistics')
		status.bar.display(cb.stat.data, TextOutputVar, 'Use all data or a spatial average or station by station to calculate the statistics')

		#################
		tkbind(cb.stat.data, "<<ComboboxSelected>>", function(){
			stateDispSTN <- if(tclvalue(stat.data) == 'Per station') 'normal' else 'disabled'
			tkconfigure(bt.stat.prev, state = stateDispSTN)
			tkconfigure(cb.stat.sel, state = stateDispSTN)
			tkconfigure(bt.stat.next, state = stateDispSTN)
			stateMaps <- if(tclvalue(stat.data) == 'Per station') 'normal' else 'disabled'
			tkconfigure(cb.stats.maps, state = stateMaps)
			tkconfigure(bt.stats.maps, state = stateMaps)
			stateStnID <- if(tclvalue(stat.data) == 'Per station') 'normal' else 'disabled'
			tkconfigure(cb.stn.graph, state = stateStnID)
			tkconfigure(bt.stn.graph.prev, state = stateStnID)
			tkconfigure(bt.stn.graph.next, state = stateStnID)

			TYPEGRAPH <- c("Scatter", "CDF", "Lines")
			if(tclvalue(stat.data) == 'All Data'){
				TYPEGRAPH <- c("Scatter", "CDF")
				if(tclvalue(EnvHOValidationplot$type.graph) == "Lines") tclvalue(EnvHOValidationplot$type.graph) <- "Scatter"
			}
			tkconfigure(cb.stats.graph, values = TYPEGRAPH)
		})

		##############################################

		frameDicho <- ttklabelframe(subfr3, text = "Dichotomous validation", relief = 'groove')

		if(clim.var == 'RR') trhesVal <- 1
		if(clim.var == 'TT') trhesVal <- 20
		dicho.thres <- tclVar(trhesVal)
		# dicho.thres <- tclVar(GeneralParameters$dicho.fcst$opr.thres)
		dicho.opr <- tclVar(GeneralParameters$dicho.fcst$opr.fun)

		txt.dicho <- tklabel(frameDicho, text = 'Threshold', anchor = 'w', justify = 'left')
		cb.dicho <- ttkcombobox(frameDicho, values = c(">=", ">", "<=", "<"), textvariable = dicho.opr, width = 4, state = 'readonly')
		en.dicho <- tkentry(frameDicho, textvariable = dicho.thres, width = 6)

		tkgrid(txt.dicho, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.dicho, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dicho, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.dicho, 'Threshold to be specified to separate "yes" and "no" events')
		status.bar.display(en.dicho, TextOutputVar, 'Threshold to be specified to separate "yes" and "no" events')

		##############################################

		bt.calc.stat <- ttkbutton(subfr3, text = "Calculate Statistics")

		tkconfigure(bt.calc.stat, command = function(){
			GeneralParameters$date.range$start.month <- which(MOIS%in%str_trim(tclvalue(start.mois)))
			GeneralParameters$date.range$end.month <- which(MOIS%in%str_trim(tclvalue(end.mois)))
			GeneralParameters$date.range$start.year <- as.numeric(str_trim(tclvalue(start.year)))
			GeneralParameters$date.range$end.year <- as.numeric(str_trim(tclvalue(end.year)))

			GeneralParameters$aggr.series$aggr.data <- switch(tclvalue(aggr.data), '0' = FALSE, '1' = TRUE)
			GeneralParameters$aggr.series$aggr.fun <- str_trim(tclvalue(aggr.fun))
			# GeneralParameters$aggr.series$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
			GeneralParameters$aggr.series$opr.fun <- str_trim(tclvalue(opr.fun))
			GeneralParameters$aggr.series$opr.thres <- as.numeric(str_trim(tclvalue(opr.thres)))

			GeneralParameters$stat.data <- switch(tclvalue(stat.data),
												'All Data' = 'all',
												'Spatial Average' = 'avg',
												'Per station' = 'stn')

			GeneralParameters$dicho.fcst$opr.thres <- as.numeric(str_trim(tclvalue(dicho.thres)))
			GeneralParameters$dicho.fcst$opr.fun <- str_trim(tclvalue(dicho.opr))

			#####
			GeneralParameters$STN.file <- str_trim(tclvalue(file.stnfl))
			GeneralParameters$outdir <- str_trim(tclvalue(file.save1))

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			InsertMessagesTxt(main.txt.out, "Validation .................")
			tcl('update')
			ret <- tryCatch(
				HandOutValidationDataProcs(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e){
					 errorFun(e)
				},
				finally = {
					tkconfigure(main.win, cursor = '')
				}
			)

			msg0 <- "Statistics calculation finished successfully"
			msg1 <- "Validation failed"
			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)

					if(tclvalue(stat.data) == 'Per station'){
						tkconfigure(cb.stat.sel, values = EnvHOValidation$opDATA$id)
						tclvalue(stn.stat.tab) <- EnvHOValidation$opDATA$id[1]

						tkconfigure(cb.stn.graph, values = EnvHOValidation$opDATA$id, state = 'normal')
						tclvalue(EnvHOValidationplot$stnIDGraph) <- EnvHOValidation$opDATA$id[1]
					}
				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})

		#############################
		tkgrid(frameHOV, row = 0, column = 0, sticky = 'we')
		tkgrid(frameSeason, row = 1, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameAggr, row = 2, column = 0, sticky = 'we', pady = 1)
		tkgrid(cb.stat.data, row = 3, column = 0, sticky = 'we', pady = 3)
		tkgrid(frameDicho, row = 4, column = 0, sticky = '', pady = 3)
		tkgrid(bt.calc.stat, row = 5, column = 0, sticky = 'we', pady = 3)

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

		frameStatTab <- ttklabelframe(subfr4, text = "Display Statistics Table", relief = 'groove')

		STATIONIDS <- ''
		stn.stat.tab <- tclVar()
		stateDispSTN <- if(GeneralParameters$stat.data == 'stn') 'normal' else 'disabled'

		bt.stat.disp <- ttkbutton(frameStatTab, text = "Display Table")
		bt.stat.prev <- ttkbutton(frameStatTab, text = "<<", state = stateDispSTN, width = 4)
		bt.stat.next <- ttkbutton(frameStatTab, text = ">>", state = stateDispSTN, width = 4)
		cb.stat.sel <- ttkcombobox(frameStatTab, values = STATIONIDS, textvariable = stn.stat.tab, width = largeur3, state = stateDispSTN)

		################
		validStatTab <- NULL

		tkconfigure(bt.stat.disp, command = function(){
			if(!is.null(EnvHOValidation$Statistics)){
				if(tclvalue(stat.data) == 'All Data'){
					don <- EnvHOValidation$Statistics$ALL
					dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics, Description = don$description)
					titleTab <- 'All-Data Statistics'
				}
				if(tclvalue(stat.data) == 'Spatial Average'){
					don <- EnvHOValidation$Statistics$AVG
					dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics, Description = don$description)
					titleTab <- 'Spatial-Average Statistics'
				}
				if(tclvalue(stat.data) == 'Per station'){
					don <- EnvHOValidation$Statistics$STN
					istn <- which(EnvHOValidation$opDATA$id == str_trim(tclvalue(stn.stat.tab)))
					dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics[, istn], Description = don$description)
					titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')
				}
				retNBTab <- tableValidationNotebookTab_unik(tknotes, dat2disp, titleTab, validStatTab, AllOpenTabType, AllOpenTabData)
				validStatTab <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.stat.prev, command = function(){
			if(!is.null(EnvHOValidation$Statistics)){
				don <- EnvHOValidation$Statistics$STN
				istn <- which(EnvHOValidation$opDATA$id == str_trim(tclvalue(stn.stat.tab)))
				istn <- istn-1
				if(istn < 1) istn <- length(EnvHOValidation$opDATA$id)
				tclvalue(stn.stat.tab) <- EnvHOValidation$opDATA$id[istn]

				dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics[, istn], Description = don$description)
				titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')

				retNBTab <- tableValidationNotebookTab_unik(tknotes, dat2disp, titleTab, validStatTab, AllOpenTabType, AllOpenTabData)
				validStatTab <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.stat.next, command = function(){
			if(!is.null(EnvHOValidation$Statistics)){
				don <- EnvHOValidation$Statistics$STN
				istn <- which(EnvHOValidation$opDATA$id == str_trim(tclvalue(stn.stat.tab)))
				istn <- istn+1
				if(istn > length(EnvHOValidation$opDATA$id)) istn <- 1
				tclvalue(stn.stat.tab) <- EnvHOValidation$opDATA$id[istn]

				dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics[, istn], Description = don$description)
				titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')

				retNBTab <- tableValidationNotebookTab_unik(tknotes, dat2disp, titleTab, validStatTab, AllOpenTabType, AllOpenTabData)
				validStatTab <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkgrid(bt.stat.disp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.stat.prev, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.stat.sel, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.stat.next, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		##############################################

		frameMap <- ttklabelframe(subfr4, text = "Statistics Maps", relief = 'groove')

		EnvHOValidationplot$statistics <- tclVar('Correlation')

		stateMaps <- if(GeneralParameters$stat.data == 'stn') 'normal' else 'disabled'

		cb.stats.maps <- ttkcombobox(frameMap, values = CHXSTATS, textvariable = EnvHOValidationplot$statistics, width = largeur2, state = stateMaps)
		bt.stats.maps <- ttkbutton(frameMap, text = "PLOT", state = stateMaps)

		EnvHOValidationplot$notebookTab.maps <- NULL
		tkconfigure(bt.stats.maps, command = function(){
			if(!is.null(EnvHOValidation$Statistics)){
				EnvHOValidationplot$xlim.maps <- range(EnvHOValidation$opDATA$lon, na.rm = TRUE)
				EnvHOValidationplot$ylim.maps <- range(EnvHOValidation$opDATA$lat, na.rm = TRUE)

				imgContainer <- HOValidation.DisplayStatMaps(tknotes)

				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvHOValidationplot$notebookTab.maps, AllOpenTabType, AllOpenTabData)
				EnvHOValidationplot$notebookTab.maps <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkgrid(cb.stats.maps, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stats.maps, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		##############################################

		frameGraph <- ttklabelframe(subfr4, text = "Graphs", relief = 'groove')

		TYPEGRAPH <- c("Scatter", "CDF", 'Lines')
		if(GeneralParameters$stat.data == 'all') TYPEGRAPH <- c("Scatter", "CDF")

		EnvHOValidationplot$type.graph <- tclVar("Scatter")
		STNIDGRAPH <- ""
		EnvHOValidationplot$stnIDGraph <- tclVar()
		stateStnID <- "disabled"

		cb.stats.graph <- ttkcombobox(frameGraph, values = TYPEGRAPH, textvariable = EnvHOValidationplot$type.graph, width = largeur2)
		bt.stats.graph <- ttkbutton(frameGraph, text = "PLOT")
		cb.stn.graph <- ttkcombobox(frameGraph, values = STNIDGRAPH, textvariable = EnvHOValidationplot$stnIDGraph, width = largeur3, state = stateStnID)
		bt.stn.graph.prev <- ttkbutton(frameGraph, text = "<<", state = stateStnID, width = 4)
		bt.stn.graph.next <- ttkbutton(frameGraph, text = ">>", state = stateStnID, width = 4)

		##############
		EnvHOValidationplot$notebookTab.graph <- NULL

		tkconfigure(bt.stats.graph, command = function(){
			if(!is.null(EnvHOValidation$Statistics)){
				imgContainer <- HOValidation.DisplayGraph(tknotes)

				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvHOValidationplot$notebookTab.graph, AllOpenTabType, AllOpenTabData)
				EnvHOValidationplot$notebookTab.graph <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.stn.graph.prev, command = function(){
			if(!is.null(EnvHOValidation$Statistics)){
				istn <- which(EnvHOValidation$opDATA$id == str_trim(tclvalue(EnvHOValidationplot$stnIDGraph)))
				istn <- istn-1
				if(istn < 1) istn <- length(EnvHOValidation$opDATA$id)
				tclvalue(EnvHOValidationplot$stnIDGraph) <- EnvHOValidation$opDATA$id[istn]

				imgContainer <- HOValidation.DisplayGraph(tknotes)

				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvHOValidationplot$notebookTab.graph, AllOpenTabType, AllOpenTabData)
				EnvHOValidationplot$notebookTab.graph <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.stn.graph.next, command = function(){
			if(!is.null(EnvHOValidation$Statistics)){
				istn <- which(EnvHOValidation$opDATA$id == str_trim(tclvalue(EnvHOValidationplot$stnIDGraph)))
				istn <- istn+1
				if(istn > length(EnvHOValidation$opDATA$id)) istn <- 1
				tclvalue(EnvHOValidationplot$stnIDGraph) <- EnvHOValidation$opDATA$id[istn]

				imgContainer <- HOValidation.DisplayGraph(tknotes)

				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvHOValidationplot$notebookTab.graph, AllOpenTabType, AllOpenTabData)
				EnvHOValidationplot$notebookTab.graph <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkgrid(cb.stats.graph, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 12, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stats.graph, row = 0, column = 12, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stn.graph.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(cb.stn.graph, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 12, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stn.graph.next, row = 1, column = 15, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		#############################
		tkgrid(frameStatTab, row = 0, column = 0, sticky = 'we')
		tkgrid(frameMap, row = 1, column = 0, sticky = 'we', pady = 3)
		tkgrid(frameGraph, row = 2, column = 0, sticky = 'we', pady = 1)

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

		EnvHOValidationplot$add.shp <- tclVar(GeneralParameters$add.to.plot$add.shp)
		file.plotShp <- tclVar(GeneralParameters$add.to.plot$shp.file)

		stateSHP <- if(GeneralParameters$add.to.plot$add.shp) "normal" else "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvHOValidationplot$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
		cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur, state = stateSHP)
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

				tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
				tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.dispShp)
				tkconfigure(cb.adddem, values = unlist(listOpenFiles), textvariable = file.grddem)
				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)

				shpofile <- getShpOpenData(file.plotShp)
				if(is.null(shpofile)) EnvHOValidationplot$shp <- NULL
				EnvHOValidationplot$shp <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.addshp, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvHOValidationplot$shp <- NULL
			EnvHOValidationplot$shp <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(EnvHOValidationplot$add.shp) == "1") "disabled" else "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
		})

		##############################################

		frameDEM <- ttklabelframe(subfr5, text = "DEM", relief = 'groove')

		EnvHOValidationplot$add.dem <- tclVar(GeneralParameters$add.to.plot$add.dem)
		file.grddem <- tclVar(GeneralParameters$add.to.plot$dem.file)

		stateDEM <- if(GeneralParameters$add.to.plot$add.dem) "normal" else "disabled"

		chk.adddem <- tkcheckbutton(frameDEM, variable = EnvHOValidationplot$add.dem, text = "Add DEM  to the Map", anchor = 'w', justify = 'left')
		cb.adddem <- ttkcombobox(frameDEM, values = unlist(listOpenFiles), textvariable = file.grddem, width = largeur, state = stateDEM)
		bt.adddem <- tkbutton(frameDEM, text = "...", state = stateDEM)

		tkconfigure(bt.adddem, command = function(){
			nc.opfiles <- getOpenNetcdf(main.win, all.opfiles)
			if(!is.null(nc.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'netcdf'
				AllOpenFilesData[[nopf+1]] <<- nc.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(file.grddem) <- AllOpenFilesData[[nopf+1]][[1]]

				tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
				tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.dispShp)
				tkconfigure(cb.adddem, values = unlist(listOpenFiles), textvariable = file.grddem)
				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)

				demData <- getDemOpenData(str_trim(tclvalue(file.grddem)), convertNeg2NA = TRUE)
				if(is.null(demData)) EnvHOValidationplot$dem <- NULL
				names(demData) <- c('x', 'y', 'z')
				EnvHOValidationplot$dem$elv <- demData

				######
				demr <- raster(demData)
				slope <- terrain(demr, opt = 'slope')
				aspect <- terrain(demr, opt = 'aspect')
				hill <- hillShade(slope, aspect, angle = 40, direction = 270)
				hill <- t(as.matrix(hill))
				hill <- hill[, rev(seq(ncol(hill)))]
				EnvHOValidationplot$dem$hill <- list(x = demData$x, y = demData$y, z = hill)
				rm(demData, demr, slope, aspect, hill)
			}else return(NULL)
		})

		tkgrid(chk.adddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.adddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.adddem, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		#################
		tkbind(cb.adddem, "<<ComboboxSelected>>", function(){
			demData <- getDemOpenData(str_trim(tclvalue(file.grddem)), convertNeg2NA = TRUE)
			if(is.null(demData)) EnvHOValidationplot$dem <- NULL
			names(demData) <- c('x', 'y', 'z')
			EnvHOValidationplot$dem$elv <- demData

			#####
			demr <- raster(demData)
			slope <- terrain(demr, opt = 'slope')
			aspect <- terrain(demr, opt = 'aspect')
			hill <- hillShade(slope, aspect, angle = 40, direction = 270)
			hill <- t(as.matrix(hill))
			hill <- hill[, rev(seq(ncol(hill)))]
			EnvHOValidationplot$dem$hill <- list(x = demData$x, y = demData$y, z = hill)
			rm(demData, demr, slope, aspect, hill)
		})

		tkbind(chk.adddem, "<Button-1>", function(){
			stateDEM <- if(tclvalue(EnvHOValidationplot$add.dem) == "1") "disabled" else "normal"
			tkconfigure(cb.adddem, state = stateDEM)
			tkconfigure(bt.adddem, state = stateDEM)
		})

		#############################
		tkgrid(frameSHP, row = 3, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameDEM, row = 4, column = 0, sticky = 'we', pady = 1)

	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}


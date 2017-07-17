
ValidationPanelCmd <- function(clim.var){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		wttkcombo <- as.integer(w.scale(30)/sfont0)
		largeur <- as.integer(w.scale(28)/sfont0)
		largeur1 <- as.integer(w.scale(30)/sfont0)
		wncdf_ff <- as.integer(w.scale(20)/sfont0)
	}else{
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46)
		wttkcombo <- as.integer(w.scale(26)/sfont0)
		largeur <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(23)/sfont0)
		wncdf_ff <- as.integer(w.scale(16)/sfont0)
	}

	GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'Hold_Out_Validation.json'))
	MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

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

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "General")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Select Stations")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Validation")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Plot")

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

	frameStn <- ttklabelframe(subfr1, text = "Gauge validation data file", relief = 'groove')

	file.period <- tclVar()
	CbperiodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$stn.file$tstep, 
									'daily' = CbperiodVAL[1], 
									'dekadal' = CbperiodVAL[2],
									'monthly' = CbperiodVAL[3])
	file.stnfl <- tclVar(GeneralParameters$stn.file$file)

	cb.tstep <- ttkcombobox(frameStn, values = CbperiodVAL, textvariable = file.period)
	cb.stnfl <- ttkcombobox(frameStn, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur)
	bt.stnfl <- tkbutton(frameStn, text = "...")

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
			tkconfigure(cb.file.ncdf, values = unlist(listOpenFiles), textvariable = file.grdCDF)
			tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.plotShp)
		}else return(NULL)
	})

	#######################

	tkgrid(cb.tstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	infobulle(cb.tstep, 'Time step of the data')
	status.bar.display(cb.tstep, TextOutputVar, 'Time step of the data')
	infobulle(cb.stnfl, 'Select the station data in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Select the file containing the station data in CDT format')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')

	#######################

	tkbind(cb.tstep, "<<ComboboxSelected>>", function(){
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
		CHXSTATS <- c('Correlation', 'Nash-Sutcliffe Efficiency', 'Bias', 'Mean Absolute Error', 'Mean Error')
		CHXSTATS1 <- c('Probability Of Detection', 'False Alarm Ratio', 'Frequency Bias', 'Critical Success Index', 'Heidke Skill Score')
		if(clim.var == 'RR' & tclvalue(file.period) == 'Daily data' & tclvalue(aggr.data) == "0"){
			CHXSTATS <- c(CHXSTATS, CHXSTATS1)
		}else{
			if(tclvalue(EnvHOValidationplot$statistics)%in%CHXSTATS1) tclvalue(EnvHOValidationplot$statistics) <- 'Correlation'
		}
		tkconfigure(cb.stats.maps, values = CHXSTATS)
	})


	#######################

	frameNcdf <- ttklabelframe(subfr1, text = "NetCDF files", relief = 'groove')

	dirNetCDF <- tclVar(GeneralParameters$ncdf.file$dir)
	file.grdCDF <- tclVar(GeneralParameters$ncdf.file$sample)
	if(clim.var == 'RR') init.ff <- "rr_mrg_%s%s%s.nc"
	if(clim.var == 'TT') init.ff <- "tmax_mrg_%s%s%s.nc"
	netCDFff <- tclVar(init.ff)

	txt.dir.ncdf <- tklabel(frameNcdf, text = "Directory of NetCDF files", anchor = 'w', justify = 'left')
	en.dir.ncdf <- tkentry(frameNcdf, textvariable = dirNetCDF, width = largeur1)
	bt.dir.ncdf <- tkbutton(frameNcdf, text = "...")
	txt.file.ncdf <- tklabel(frameNcdf, text = "NetCDF's sample file", anchor = 'w', justify = 'left')
	cb.file.ncdf <- ttkcombobox(frameNcdf, values = unlist(listOpenFiles), textvariable = file.grdCDF, width = largeur)
	bt.file.ncdf <- tkbutton(frameNcdf, text = "...")
	txt.ff.ncdf <- tklabel(frameNcdf, text = "NetCDF file format", anchor = 'e', justify = 'right')
	en.ff.ncdf <- tkentry(frameNcdf, textvariable = netCDFff, width = wncdf_ff)

	#######################
	tkconfigure(bt.dir.ncdf, command = function(){
		dir4cdf <- tk_choose.dir(tclvalue(dirNetCDF), "")
		tclvalue(dirNetCDF) <- if(is.na(dir4cdf)) "" else dir4cdf
	})

	tkconfigure(bt.file.ncdf, command = function(){
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles, initialdir = tclvalue(dirNetCDF))
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdCDF) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.file.ncdf, values = unlist(listOpenFiles), textvariable = file.grdCDF)
			tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.plotShp)
		}else return(NULL)
	})

	#############################
	tkgrid(txt.dir.ncdf, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.dir.ncdf, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.ncdf, row = 1, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.file.ncdf, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.file.ncdf, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.ncdf, row = 3, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.ff.ncdf, row = 4, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.ff.ncdf, row = 4, column = 2, sticky = 'w', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.dir.ncdf, 'Enter the full path to directory containing the NetCDF files')
	status.bar.display(en.dir.ncdf, TextOutputVar, 'Enter the full path to directory containing the NetCDF files')
	infobulle(en.ff.ncdf, 'Enter the format of the NetCDF files names, example: rr_mrg_1983011.nc')
	status.bar.display(en.ff.ncdf, TextOutputVar, 'Enter the format of the NetCDF files names, example: rr_mrg_1983011.nc')
	infobulle(cb.file.ncdf, 'Select the file in the list')
	status.bar.display(cb.file.ncdf, TextOutputVar, 'File containing a sample of NetCDF data')
	infobulle(bt.file.ncdf, 'Browse file if not listed')
	status.bar.display(bt.file.ncdf, TextOutputVar, 'Browse file if not listed')

	#######################

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
	tkgrid(frameStn, row = 0, column = 0, sticky = 'we')
	tkgrid(frameNcdf, row = 1, column = 0, sticky = 'we', pady = 3)
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

	#######################

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
				shpfopen <- getShpOpenData(file.plotShp)
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

	#######################

	frameShp <- ttklabelframe(subfr2, text = "Boundaries Shapefiles", relief = 'groove')

	file.plotShp <- tclVar(GeneralParameters$shp.file$shp)
	shpAttr <- tclVar(GeneralParameters$shp.file$attr)
	EnvHOValidationplot$namePoly <- tclVar()

	cb.shpF <- ttkcombobox(frameShp, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur)
	bt.shpF <- tkbutton(frameShp, text = "...")
	txt.attr.shpF <- tklabel(frameShp, text = "Attribute field to be used and displayed", anchor = 'w', justify = 'left')
	EnvHOValidationplot$cb.shpAttr <- ttkcombobox(frameShp, values='', textvariable = shpAttr, width = wttkcombo, state = 'disabled')
	cb.Polygon <- ttkcombobox(frameShp, values='', textvariable = EnvHOValidationplot$namePoly, width = wttkcombo, state = 'disabled')

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
			tclvalue(file.plotShp) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.plotShp)
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.file.ncdf, values = unlist(listOpenFiles), textvariable = file.grdCDF)

			###
			shpf <- getShpOpenData(file.plotShp)
			dat <- shpf[[2]]@data
			AttrTable <- names(dat)
			tclvalue(shpAttr) <- AttrTable[1]

			adminN <- as.character(dat[, 1])
			name.poly <- levels(as.factor(adminN))
			tclvalue(EnvHOValidationplot$namePoly) <- name.poly[1]

			tkconfigure(EnvHOValidationplot$cb.shpAttr, values = AttrTable, textvariable = shpAttr)
			tkconfigure(cb.Polygon, values = name.poly, textvariable = EnvHOValidationplot$namePoly)
		}
	})

	#######################
	tkbind(cb.shpF, "<<ComboboxSelected>>", function(){
		shpf <- getShpOpenData(file.plotShp)
		if(!is.null(shpf)){
			dat <- shpf[[2]]@data
			AttrTable <- names(dat)
			tclvalue(shpAttr) <- AttrTable[1]
			adminN <- as.character(dat[, as.numeric(tclvalue(tcl(EnvHOValidationplot$cb.shpAttr, 'current')))+1])
			name.poly <- levels(as.factor(adminN))
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
		shpf <- getShpOpenData(file.plotShp)
		if(!is.null(shpf)){
			dat <- shpf[[2]]@data
			adminN <- as.character(dat[, as.numeric(tclvalue(tcl(EnvHOValidationplot$cb.shpAttr, 'current')))+1])
			name.poly <- levels(as.factor(adminN))
		}else{
			name.poly <- ''
		}
		tclvalue(EnvHOValidationplot$namePoly) <- name.poly[1]
		tkconfigure(cb.Polygon, values = name.poly, textvariable = EnvHOValidationplot$namePoly)
	})

	########################
	tkbind(cb.Polygon, "<<ComboboxSelected>>", function(){
		if(tclvalue(EnvHOValidationplot$namePoly) != ''){
			shpfopen <- getShpOpenData(file.plotShp)
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


	#######################

	frameZoom <- ttklabelframe(subfr2, text = "ZOOM", relief = 'groove')

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

	#######################

	frameDisp <- tkframe(subfr2)

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
		shpofile <- getShpOpenData(file.plotShp)
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
			InsertMessagesTxt(main.txt.out, 'Provide the station data', format = TRUE)
		}
		if(tclvalue(EnvHOValidationplot$type.select) == 'Polygons' & plotOK){
			if(!is.null(shpofile)){
				shpf <- shpofile[[2]]
				lo1 <- min(lo1, round(bbox(shpf)[1, 1], 4))
				lo2 <- max(lo2, round(bbox(shpf)[1, 2], 4))
				la1 <- min(la1, round(bbox(shpf)[2, 1], 4))
				la2 <- max(la2, round(bbox(shpf)[2, 2], 4))
				plotOK <- TRUE
			}else{
				plotOK <- FALSE
				InsertMessagesTxt(main.txt.out, 'Provide the ESRI shapfile for for administrative boundaries', format = TRUE)
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

	tkgrid(frameSelect, row = 0, column = 0, sticky = 'we', columnspan = 6, pady = 5)
	tkgrid(frameShp, row = 1, column = 0, sticky = 'we', columnspan = 6)
	tkgrid(frameZoom, row = 2, column = 0, sticky = 'ns', columnspan = 2, ipady = 5)
	tkgrid(frameDisp, row = 2, column = 2, sticky = 'we', columnspan = 4)

	##########################
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

	#######################

	frameSeason <- ttklabelframe(subfr3, text = "Years & Season", relief = 'groove')

	mon1 <- as.numeric(str_trim(GeneralParameters$date.range$start.month))
	mon2 <- as.numeric(str_trim(GeneralParameters$date.range$end.month))
	start.mois <- tclVar(MOIS[mon1])
	end.mois <- tclVar(MOIS[mon2])
	start.year <- tclVar(GeneralParameters$date.range$start.year)
	end.year <- tclVar(GeneralParameters$date.range$end.year)

	txt.year <- tklabel(frameSeason, text = 'Years')
	txt.month <- tklabel(frameSeason, text = 'Months')
	txt.start <- tklabel(frameSeason, text = 'Start', anchor = 'e', justify = 'right')
	txt.end <- tklabel(frameSeason, text = 'End', anchor = 'e', justify = 'right')
	en.years1 <- tkentry(frameSeason, width = 5, textvariable = start.year, justify = 'right')
	en.years2 <- tkentry(frameSeason, width = 5, textvariable = end.year, justify = 'right')
	cb.month1 <- ttkcombobox(frameSeason, values = MOIS, textvariable = start.mois, width = 15)
	cb.month2 <- ttkcombobox(frameSeason, values = MOIS, textvariable = end.mois, width = 15)

	tkgrid(txt.year, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(txt.month, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

	tkgrid(txt.start, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(txt.end, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

	tkgrid(en.years1, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(en.years2, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(cb.month1, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 1)
	tkgrid(cb.month2, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 1)

	infobulle(en.years1, 'Start year of the period to calculate the statistics')
	status.bar.display(en.years1, TextOutputVar, 'Start year of the period to calculate the statistics')
	infobulle(en.years2, 'End year of the period to calculate the statistics')
	status.bar.display(en.years2, TextOutputVar, 'End year of the period to calculate the statistics')
	infobulle(cb.month1, 'Start month of the period to calculate the statistics')
	status.bar.display(cb.month1, TextOutputVar, 'Start month of the period to calculate the statistics')
	infobulle(cb.month2, 'End month of the season to calculate the statistics')
	status.bar.display(cb.month2, TextOutputVar, 'End month of the season to calculate the statistics')

	#############################

	frameAggr <- ttklabelframe(subfr3, text = "Data aggregation", relief = 'groove')

	aggr.data <- tclVar(GeneralParameters$aggr.series$aggr.data)
	aggr.fun <- tclVar(GeneralParameters$aggr.series$aggr.fun)
	# min.frac <- tclVar(GeneralParameters$aggr.series$min.frac)
	opr.fun <- tclVar(GeneralParameters$aggr.series$opr.fun)
	opr.thres <- tclVar(GeneralParameters$aggr.series$opr.thres)

	AGGREGFUN <- c("mean", "sum", "count")
	if(GeneralParameters$stn.file$tstep != 'daily' & !GeneralParameters$aggr.series$aggr.data) AGGREGFUN <- AGGREGFUN[-3]
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
	en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = 6, width = 6, state = stateo2)

	tkgrid(chk.aggrdata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.aggfun, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.aggfun, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	# tkgrid(txt.minfrac, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	# tkgrid(en.minfrac, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.opfun, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.opfun, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.opthres, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.opthres, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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

		CHXSTATS <- c('Correlation', 'Nash-Sutcliffe Efficiency', 'Bias', 'Mean Absolute Error', 'Mean Error')
		CHXSTATS1 <- c('Probability Of Detection', 'False Alarm Ratio', 'Frequency Bias', 'Critical Success Index', 'Heidke Skill Score')
		if(clim.var == 'RR' & tclvalue(file.period) == 'Daily data' & tclvalue(aggr.data) == "1"){
			CHXSTATS <- c(CHXSTATS, CHXSTATS1)
		}else{
			if(tclvalue(EnvHOValidationplot$statistics)%in%CHXSTATS1) tclvalue(EnvHOValidationplot$statistics) <- 'Correlation'
		}
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
	})

	#############################

	bt.calc.stat <- ttkbutton(subfr3, text = "Calculate Statistics")

	tkconfigure(bt.calc.stat, command = function(){
		GeneralParameters$clim.var <- clim.var
		GeneralParameters$stn.file$tstep <- switch(tclvalue(file.period),
		 									'Daily data' = 'daily',
											'Dekadal data' =  'dekadal',
											'Monthly data' = 'monthly')
		GeneralParameters$stn.file$file <- str_trim(tclvalue(file.stnfl))

		GeneralParameters$ncdf.file$dir <- str_trim(tclvalue(dirNetCDF))
		GeneralParameters$ncdf.file$sample <- str_trim(tclvalue(file.grdCDF))
		GeneralParameters$ncdf.file$format <- str_trim(tclvalue(netCDFff))
		GeneralParameters$outdir <- str_trim(tclvalue(file.save1))

		GeneralParameters$shp.file$shp <- str_trim(tclvalue(file.plotShp))
		GeneralParameters$shp.file$attr <- str_trim(tclvalue(shpAttr))

		GeneralParameters$type.select <- switch(tclvalue(EnvHOValidationplot$type.select),
													'All Stations' = 'all',
													'Rectangle' = 'rect',
													'Polygons' = 'poly')

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

		GeneralParameters$Geom <- NULL
		GeneralParameters$Geom$minlon <- as.numeric(str_trim(tclvalue(EnvHOValidationplot$minlonRect)))
		GeneralParameters$Geom$maxlon <- as.numeric(str_trim(tclvalue(EnvHOValidationplot$maxlonRect)))
		GeneralParameters$Geom$minlat <- as.numeric(str_trim(tclvalue(EnvHOValidationplot$minlatRect)))
		GeneralParameters$Geom$maxlat <- as.numeric(str_trim(tclvalue(EnvHOValidationplot$maxlatRect)))
		GeneralParameters$Geom$namePoly <- str_trim(tclvalue(EnvHOValidationplot$namePoly))

		assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

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

		if(!is.null(ret)){
			if(ret == 0){
				InsertMessagesTxt(main.txt.out, "Validation finished successfully")

				if(tclvalue(stat.data) == 'Per station'){
					tkconfigure(cb.stat.sel, values = EnvHOValidation$opDATA$id)
					tclvalue(stn.stat.tab) <- EnvHOValidation$opDATA$id[1]

					tkconfigure(cb.stn.graph, values = EnvHOValidation$opDATA$id, state = 'normal')
					tclvalue(EnvHOValidationplot$stnIDGraph) <- EnvHOValidation$opDATA$id[1]
				}
			}else InsertMessagesTxt(main.txt.out, "Validation failed", format = TRUE)
		}else InsertMessagesTxt(main.txt.out, "Validation failed", format = TRUE)
	})

	#############################

	frameStatTab <- ttklabelframe(subfr3, text = "Display Statistics Table", relief = 'groove')

	STATIONIDS <- ''
	stn.stat.tab <- tclVar()
	stateDispSTN <- if(GeneralParameters$stat.data == 'stn') 'normal' else 'disabled'

	bt.stat.disp <- ttkbutton(frameStatTab, text = "Display Table")
	bt.stat.prev <- ttkbutton(frameStatTab, text = "<<", state = stateDispSTN, width = 4)
	bt.stat.next <- ttkbutton(frameStatTab, text = ">>", state = stateDispSTN, width = 4)
	cb.stat.sel <- ttkcombobox(frameStatTab, values = STATIONIDS, textvariable = stn.stat.tab, width = 20, state = stateDispSTN)

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
				istn <- which(EnvHOValidation$opDATA$id == tclvalue(stn.stat.tab))
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
			istn <- which(EnvHOValidation$opDATA$id == tclvalue(stn.stat.tab))
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
			istn <- which(EnvHOValidation$opDATA$id == tclvalue(stn.stat.tab))
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

	#############################
	tkgrid(frameSeason, row = 0, column = 0, sticky = 'we')
	tkgrid(frameAggr, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(cb.stat.data, row = 2, column = 0, sticky = 'we', pady = 3)
	tkgrid(bt.calc.stat, row = 3, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameStatTab, row = 4, column = 0, sticky = 'we', pady = 3)

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

	frameMap <- ttklabelframe(subfr4, text = "Statistics Maps", relief = 'groove')

	EnvHOValidationplot$statistics <- tclVar('Correlation')
	CHXSTATS <- c('Correlation', 'Nash-Sutcliffe Efficiency', 'Bias', 'Mean Absolute Error', 'Mean Error')
	if(clim.var == 'RR' & GeneralParameters$stn.file$tstep == 'daily' & !GeneralParameters$aggr.series$aggr.data)
		CHXSTATS <- c(CHXSTATS, 'Probability Of Detection', 'False Alarm Ratio', 'Frequency Bias', 'Critical Success Index', 'Heidke Skill Score')
	stateMaps <- if(GeneralParameters$stat.data == 'stn') 'normal' else 'disabled'

	cb.stats.maps <- ttkcombobox(frameMap, values = CHXSTATS, textvariable = EnvHOValidationplot$statistics, width = 21, state = stateMaps)
	bt.stats.maps <- ttkbutton(frameMap, text = "PLOT", state = stateMaps)

	EnvHOValidationplot$notebookTab.maps <- NULL
	tkconfigure(bt.stats.maps, command = function(){
		if(!is.null(EnvHOValidation$Statistics)){
			shpofile <- getShpOpenData(file.plotShp)
			ocrds <- getBoundaries(shpofile[[2]])
			# EnvHOValidationplot$xlim.maps <- range(c(ocrds[, 1], EnvHOValidation$opDATA$lon), na.rm = TRUE)
			# EnvHOValidationplot$ylim.maps <- range(c(ocrds[, 2], EnvHOValidation$opDATA$lat), na.rm = TRUE)

			EnvHOValidationplot$xlim.maps <- range(EnvHOValidation$opDATA$lon, na.rm = TRUE)
			EnvHOValidationplot$ylim.maps <- range(EnvHOValidation$opDATA$lat, na.rm = TRUE)

			imgContainer <- HOValidation.DisplayStatMaps(tknotes, ocrds)

			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvHOValidationplot$notebookTab.maps, AllOpenTabType, AllOpenTabData)
			EnvHOValidationplot$notebookTab.maps <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkgrid(cb.stats.maps, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(bt.stats.maps, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	#######################

	frameGraph <- ttklabelframe(subfr4, text = "Graphs", relief = 'groove')

	TYPEGRAPH <- c("Scatter", "CDF")
	EnvHOValidationplot$type.graph <- tclVar("Scatter")
	STNIDGRAPH <- ""
	EnvHOValidationplot$stnIDGraph <- tclVar()
	stateStnID <- "disabled"

	cb.stats.graph <- ttkcombobox(frameGraph, values = TYPEGRAPH, textvariable = EnvHOValidationplot$type.graph, width = 21)
	bt.stats.graph <- ttkbutton(frameGraph, text = "PLOT")
	txt.stn.graph <- tklabel(frameGraph, text = "Station",  anchor = 'e', justify = 'right')
	cb.stn.graph <- ttkcombobox(frameGraph, values = STNIDGRAPH, textvariable = EnvHOValidationplot$stnIDGraph, state = stateStnID)

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

	tkgrid(cb.stats.graph, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 12, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(bt.stats.graph, row = 0, column = 12, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(txt.stn.graph, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(cb.stn.graph, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 15, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	#############################
	tkgrid(frameMap, row = 0, column = 0, sticky = 'we')
	tkgrid(frameGraph, row = 1, column = 0, sticky = 'we', pady = 3)

	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}


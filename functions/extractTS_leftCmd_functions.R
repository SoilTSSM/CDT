
ExtractDataPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)

		largeur1 <- as.integer(w.scale(30)/sfont0)
		largeur2 <- as.integer(w.scale(28)/sfont0)
		largeur3 <- as.integer(w.scale(14)/sfont0)
		largeur4 <- as.integer(w.scale(32)/sfont0)
		largeur5 <- as.integer(w.scale(28)/sfont0)
	}else{
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(47)

		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)
		largeur3 <- as.integer(w.scale(14)/sfont0)
		largeur4 <- as.integer(w.scale(19)/sfont0)
		largeur5 <- as.integer(w.scale(23)/sfont0)
	}

	GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'Extract_time_series.json'))
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

	EnvExtractData$pressGetCoords <- tclVar(0)

	ZoomXYval0 <- NULL

	notebookTab <- NULL

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Time series")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Location")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Output")
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

	##########################################

	frameTimeS <- ttklabelframe(subfr1, text = "Time step of input data", relief = 'groove')

	file.period <- tclVar()
	CbperiodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$in.series, 
									'daily' = CbperiodVAL[1], 
									'dekadal' = CbperiodVAL[2],
									'monthly' = CbperiodVAL[3])

	cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = file.period, width = largeur1)

	tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.fperiod, 'Select the time step of the data')
	status.bar.display(cb.fperiod, TextOutputVar, 'Select the time step of the data')

	#################
	tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
		OUTSeries <- c('Daily', 'Dekadal', 'Monthly', '3-Months', '6-Months', 'Annual')
		if(tclvalue(file.period) == 'Daily data'){
			stateDR <- 'normal'
			tclvalue(DayDek.txtVar) <- 'Day'
		}
		if(tclvalue(file.period) == 'Dekadal data'){
			stateDR <- 'normal'
			OUTSeries <- OUTSeries[-1]
			tclvalue(DayDek.txtVar) <- 'Dek'
			tclvalue(out.series) <- if(tclvalue(out.series) == 'Daily') 'Dekadal' else tclvalue(out.series)
 			tclvalue(istart.day) <- if(as.numeric(tclvalue(istart.day)) > 3) 3 else tclvalue(istart.day)
			tclvalue(iend.day) <- if(as.numeric(tclvalue(iend.day)) > 3) 3 else tclvalue(iend.day)
		}
		if(tclvalue(file.period) == 'Monthly data'){
			stateDR <- 'disabled'
			OUTSeries <- OUTSeries[-(1:2)]
			tclvalue(out.series) <- if(tclvalue(out.series) %in% c('Daily', 'Dekadal')) 'Monthly' else tclvalue(out.series)
		}

		tkconfigure(day1.v, state = stateDR)
		tkconfigure(day2.v, state = stateDR)
		tkconfigure(cb.outTS, values = OUTSeries)

		####
		AGGREGFUN <- c("mean", "sum", "count")
		if((tclvalue(out.series) == 'Daily' & tclvalue(file.period) == 'Daily data') |
			(tclvalue(out.series) == 'Dekadal' & tclvalue(file.period) == 'Dekadal data') |
			(tclvalue(out.series) == 'Monthly' & tclvalue(file.period) == 'Monthly data')){
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
		tkconfigure(en.minfrac, state = stateo0b)
		tkconfigure(cb.opfun, state = stateo1)
		tkconfigure(en.opthres, state = stateo2)

		####
		stateClim2 <- if(tclvalue(file.period) == 'Daily data' & tclvalue(type.series) != 'Raw Time Series') "normal" else "disable"
		tkconfigure(en.winsize, state = stateClim2)
	})

	#############################
	frameNC <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

	ncDIR <- tclVar(GeneralParameters$ncdf.file$dir)
	ncSample <- tclVar(GeneralParameters$ncdf.file$sample)
	ncFormat <- tclVar(GeneralParameters$ncdf.file$format)

	txt.ncdr <- tklabel(frameNC, text = 'Directory containing the NetCDF data', anchor = 'w', justify = 'left')
	en.ncdr <- tkentry(frameNC, textvariable = ncDIR, width = largeur2)
	bt.ncdr <- tkbutton(frameNC, text = "...")
	txt.ncfl <- tklabel(frameNC, text = "NetCDF data sample file", anchor = 'w', justify = 'left')
	cb.ncfl <- ttkcombobox(frameNC, values = unlist(listOpenFiles), textvariable = ncSample, width = largeur1)
	bt.ncfl <- tkbutton(frameNC, text = "...")
	txt.ncff <- tklabel(frameNC, text = "Filename format", anchor = 'e', justify = 'right')
	en.ncff <- tkentry(frameNC, textvariable = ncFormat, width = largeur3)

	#################
	tkconfigure(bt.ncdr, command = function(){
		dir4cdf <- tk_choose.dir(getwd(), "")
		tclvalue(ncDIR) <- if(is.na(dir4cdf)) "" else dir4cdf
	})

	tkconfigure(bt.ncfl, command = function(){
		initialdir <- if(file.exists(tclvalue(ncDIR))) tclvalue(ncDIR) else getwd()
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles, initialdir = initialdir)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(ncSample) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.ncfl, values = unlist(listOpenFiles), textvariable = ncSample)
			tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = shpFile)
		}else return(NULL)
	})

	tkgrid(txt.ncdr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.ncdr, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.ncdr, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.ncfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.ncfl, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.ncfl, row = 3, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.ncff, row = 4, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.ncff, row = 4, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.ncdr, 'Enter the full path to directory containing the NetCDF data')
	status.bar.display(en.ncdr, TextOutputVar, 'Enter the full path to directory containing the NetCDF data')
	status.bar.display(cb.ncfl, TextOutputVar, 'File containing a sample of the data in netcdf')
	infobulle(bt.ncfl, 'Browse file if not listed')
	infobulle(en.ncff, 'Enter the filename format of netcdf data,\nexample: rr_mrg_19830125_CLM.nc')
	status.bar.display(en.ncff, TextOutputVar, 'Enter the filename format of netcdf data,\nexample: rr_mrg_19830125_CLM.nc')

	#############################

	frameSHP <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

	shpFile <- tclVar(GeneralParameters$shp.file$shp)
	shpAttr <- tclVar(GeneralParameters$shp.file$attr)

	txt.shpF <- tklabel(frameSHP, text = "Shapefile for Administrative Boundaries", anchor = 'w', justify = 'left')
	cb.shpF <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = shpFile, width = largeur1)
	bt.shpF <- tkbutton(frameSHP, text = "...")
	txt.shpAttr <- tklabel(frameSHP, text = "Attribute field to be used and displayed", anchor = 'w', justify = 'left')
	EnvExtractData$cb.shpAttr <- ttkcombobox(frameSHP, values = '', textvariable = shpAttr)
	# adminVar.tab1 <<- ttkcombobox(frameSHP, values = '', textvariable = shpAtrr)

	#################
	tkconfigure(bt.shpF, command = function(){
		shp.opfiles <- getOpenShp(main.win, all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'shp'
			AllOpenFilesData[[nopf+1]] <<- shp.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(shpFile) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = shpFile)
			tkconfigure(cb.ncfl, values = unlist(listOpenFiles), textvariable = ncSample)

			shpf <- getShpOpenData(shpFile)
			AttrTable <- names(shpf[[2]]@data)
			tclvalue(shpAttr) <- AttrTable[1]
			tkconfigure(EnvExtractData$cb.shpAttr, values = AttrTable, textvariable = shpAttr)
		}
	})

	tkgrid(txt.shpF, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.shpF, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.shpF, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.shpAttr, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(EnvExtractData$cb.shpAttr, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################
	tkbind(cb.shpF, "<<ComboboxSelected>>", function(){
		shpf <- getShpOpenData(shpFile)
		dat <- shpf[[2]]@data
		AttrTable <- names(dat)
		tclvalue(shpAttr) <- AttrTable[1]
		tkconfigure(EnvExtractData$cb.shpAttr, values = AttrTable, textvariable = shpAttr)
	})

	tkbind(EnvExtractData$cb.shpAttr, "<<ComboboxSelected>>", function(){
		shpf <- getShpOpenData(shpFile)
		dat <- shpf[[2]]@data
		adminN <- as.character(dat[, as.numeric(tclvalue(tcl(EnvExtractData$cb.shpAttr, 'current')))+1])
		AttrTable <- levels(as.factor(adminN))
		tclvalue(EnvExtractData$namePoly) <- AttrTable[1]
		tkconfigure(cb.Polygon, values = AttrTable, textvariable = EnvExtractData$namePoly)
	})

	#############################

	openAttrSHP <- ttkbutton(subfr1, text = "Open Attribute Table")
	displayMapSHP <- ttkbutton(subfr1, text = "Display Map", width = 15)

	#################
	tkconfigure(openAttrSHP, command = function(){
		shpf <- getShpOpenData(shpFile)
		if(!is.null(shpf)){
			onglet <- addNewTab(tknotes, tab.title = shpf[[1]])
			dtab <- tclArrayVar(shpf[[2]]@data)
			table1 <- displayTable(onglet[[2]], tclArray = dtab)
			tab.array <- list(onglet, table1, shpf[[3]])

			ntab <- length(AllOpenTabType)
			AllOpenTabType[[ntab+1]] <<- 'arr'
			AllOpenTabData[[ntab+1]] <<- tab.array
			tkselect(tknotes, ntab)
		}
	})

	tkconfigure(displayMapSHP, command = function(){
		shpofile <- getShpOpenData(shpFile)
		if(!is.null(shpofile)){
			shpf <- shpofile[[2]]
			lo1 <- round(bbox(shpf)[1, 1], 4)
			lo2 <- round(bbox(shpf)[1, 2], 4)
			la1 <- round(bbox(shpf)[2, 1], 4)
			la2 <- round(bbox(shpf)[2, 2], 4)
			ZoomXYval0 <<- c(lo1, lo2, la1, la2)
			tclvalue(EnvZoomPars$xx1) <- lo1
			tclvalue(EnvZoomPars$xx2) <- lo2
			tclvalue(EnvZoomPars$yy1) <- la1
			tclvalue(EnvZoomPars$yy2) <- la2
			ZoomXYval <- as.numeric(c(tclvalue(EnvZoomPars$xx1), tclvalue(EnvZoomPars$xx2),
									  tclvalue(EnvZoomPars$yy1), tclvalue(EnvZoomPars$yy2)))

			imgContainer <- displayMap4Extraction(tknotes, shpf, ZoomXYval, notebookTab)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab, AllOpenTabType, AllOpenTabData)
			notebookTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}else InsertMessagesTxt(main.txt.out, 'Provide the ESRI shapfile of the administrative boundaries', format = TRUE)
	})

	#############################
	tkgrid(frameTimeS, row = 0, column = 0, columnspan = 2, sticky = 'we')
	tkgrid(frameNC, row = 1, column = 0, columnspan = 2, sticky = 'we', pady = 3)
	tkgrid(frameSHP, row = 2, column = 0, columnspan = 2, sticky = 'we', pady = 3)
	tkgrid(openAttrSHP, row = 3, column = 0, columnspan = 1, sticky = 'we', pady = 3)
	tkgrid(displayMapSHP, row = 3, column = 1, columnspan = 1, sticky = 'we', pady = 3)

	#######################################################################################################

	#Tab2
	frTab2 <- tkframe(cmd.tab2) 
	tkgrid(frTab2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)
	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr2, 0, weight = 1)

	##########################################

	frameDate <- ttklabelframe(subfr2, text = "Date Range", relief = 'groove')

	DayDek.txtVar <- tclVar('Dek')
	istart.yrs <- tclVar(GeneralParameters$date.range$start.year)
	istart.mon <- tclVar(GeneralParameters$date.range$start.mon)
	istart.day <- tclVar(GeneralParameters$date.range$start.day)
	iend.yrs <- tclVar(GeneralParameters$date.range$end.year)
	iend.mon <- tclVar(GeneralParameters$date.range$end.mon)
	iend.day <- tclVar(GeneralParameters$date.range$end.day)

	mon1 <- as.numeric(str_trim(GeneralParameters$date.range$start.month))
	mon2 <- as.numeric(str_trim(GeneralParameters$date.range$end.month))
	startMonth <- tclVar(MOIS[mon1])
	endMonth <- tclVar(MOIS[mon2])

	if(GeneralParameters$in.series == 'dekadal'){
		stateDR <- "normal"
		istart.day <- if(as.numeric(str_trim(GeneralParameters$date.range$start.day)) > 3) tclVar(3) else istart.day
		iend.day <- if(as.numeric(str_trim(GeneralParameters$date.range$end.day)) > 3) tclVar(3) else iend.day
	}else if(GeneralParameters$in.series == 'monthly'){
		stateDR <- "disabled"
	}else stateDR <- "normal"

	if(GeneralParameters$out.series$out.series %in% c('seasonal3', 'seasonal6', 'annual')){
		stateMon <- "disabled"
		tclvalue(startMonth) <- MOIS[as.numeric(str_trim(GeneralParameters$out.series$start.seas))]
		tclvalue(endMonth) <- MOIS[as.numeric(str_trim(GeneralParameters$out.series$end.seas))]
	}else stateMon <- "normal"

	deb.txt <- tklabel(frameDate, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frameDate, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frameDate, text = 'Year')
	mon.txt <- tklabel(frameDate, text = 'Month')
	day.txt <- tklabel(frameDate, text = tclvalue(DayDek.txtVar), textvariable = DayDek.txtVar)

	yrs1.v <- tkentry(frameDate, width = 4, textvariable = istart.yrs, justify = "left")
	mon1.v <- tkentry(frameDate, width = 4, textvariable = istart.mon, justify = "left")
	day1.v <- tkentry(frameDate, width = 4, textvariable = istart.day, justify = "left", state = stateDR)
	yrs2.v <- tkentry(frameDate, width = 4, textvariable = iend.yrs, justify = "left")
	mon2.v <- tkentry(frameDate, width = 4, textvariable = iend.mon, justify = "left")
	day2.v <- tkentry(frameDate, width = 4, textvariable = iend.day, justify = "left", state = stateDR)

	txt.month <- tklabel(frameDate, text = "Months to extract", anchor = 'w', justify = 'left')
	fr.Month <- tkframe(frameDate)

	txt.startMonth <- tklabel(fr.Month, text = "From",  anchor = 'e', justify = 'right')
	cb.startMonth <- ttkcombobox(fr.Month, values = MOIS, textvariable = startMonth, width = 10, state = stateMon)
	txt.endMonth <- tklabel(fr.Month, text = "to")
	cb.endMonth <- ttkcombobox(fr.Month, values = MOIS, textvariable = endMonth, width = 10, state = stateMon)

	tkgrid(txt.startMonth, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.startMonth, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.endMonth, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.endMonth, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(deb.txt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fin.txt, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day.txt, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day1.v, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs2.v, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon2.v, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day2.v, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.month, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.Month, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#############################

	frameTSOut <- ttklabelframe(subfr2, text = "Output time step", relief = 'groove')

	OUTSeries <- c('Daily', 'Dekadal', 'Monthly', '3-Months', '6-Months', 'Annual')
	if(GeneralParameters$in.series == 'dekadal') OUTSeries <- OUTSeries[-1]
	if(GeneralParameters$in.series == 'monthly') OUTSeries <- OUTSeries[-(1:2)]
	out.series <- tclVar()
	tclvalue(out.series) <- switch(GeneralParameters$out.series$out.series, 
									'daily' = OUTSeries[1],
									'dekadal' = OUTSeries[2],
									'monthly' = OUTSeries[3],
									'seasonal3' = OUTSeries[4], 
									'seasonal6' = OUTSeries[5], 
									'annual' = OUTSeries[6])

	mon.s <- as.numeric(str_trim(GeneralParameters$out.series$start.seas))
	start.seas <- tclVar(MOIS[mon.s])

	if(GeneralParameters$out.series$out.series %in% c('seasonal3', 'seasonal6', 'annual')){
		if(GeneralParameters$out.series$out.series == 'seasonal3') lenSeas <- 2
		if(GeneralParameters$out.series$out.series == 'seasonal6') lenSeas <- 5
		if(GeneralParameters$out.series$out.series == 'annual') lenSeas <- 11
		mon.e <- (mon.s + lenSeas)%%12
		mon.e[mon.e == 0] <- 12
	}else mon.e <- as.numeric(str_trim(GeneralParameters$out.series$end.seas))
	end.seas <- tclVar(MOIS[mon.e])

	stateTSOut <- if(GeneralParameters$in.series %in% c('daily', 'dekadal', 'monthly')) "disabled" else "normal"

	txt.outTS <- tklabel(frameTSOut, text = "Time step",  anchor = 'e', justify = 'right')
	cb.outTS <- ttkcombobox(frameTSOut, values = OUTSeries, textvariable = out.series, width = 10)
	txt.startSeas <- tklabel(frameTSOut, text = 'Start month', anchor = 'e', justify = 'right')
	cb.startSeas <- ttkcombobox(frameTSOut, values = MOIS, textvariable = start.seas, width = 10, state = stateTSOut)
	cb.endSeas <- ttkcombobox(frameTSOut, values = MOIS, textvariable = end.seas, width = 10, state = "disabled")

	tkgrid(txt.outTS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.outTS, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.startSeas, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.startSeas, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.endSeas, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.outTS, "Select the time step of the output time series")
	status.bar.display(cb.outTS, TextOutputVar, "Select the time step of the output time series")
	infobulle(cb.startSeas, 'Select the start month of the season or year')
	status.bar.display(cb.startSeas, TextOutputVar, 'Select the start month of the season or year')

	#################
	tkbind(cb.outTS, "<<ComboboxSelected>>", function(){
		if(tclvalue(out.series) %in% c('Daily', 'Dekadal', 'Monthly')){
			stateTSOut <- "disabled"
			stateMon <- "normal"

			startMonth.v <- MOIS[1]
			endMonth.v <- MOIS[12]
		}else{
			stateTSOut <- "normal"
			stateMon <- "disabled"

			if(tclvalue(out.series) == '3-Months') len <- 2
			if(tclvalue(out.series) == '6-Months') len <- 5
			if(tclvalue(out.series) == 'Annual') len <- 11
			mon1 <- which(MOIS%in%str_trim(tclvalue(start.seas)))
			mon1 <- (mon1 + len)%%12
			mon1[mon1 == 0] <- 12
			tclvalue(end.seas) <- MOIS[mon1]

			startMonth.v <- tclvalue(start.seas)
			endMonth.v <- tclvalue(end.seas)
		}

		tkconfigure(cb.startSeas, state = stateTSOut)
		tkconfigure(cb.startMonth, state = stateMon)
		tkconfigure(cb.endMonth, state = stateMon)
		tclvalue(startMonth) <- startMonth.v
		tclvalue(endMonth) <- endMonth.v

		######
		AGGREGFUN <- c("mean", "sum", "count")
		if((tclvalue(out.series) == 'Daily' & tclvalue(file.period) == 'Daily data') |
			(tclvalue(out.series) == 'Dekadal' & tclvalue(file.period) == 'Dekadal data') |
			(tclvalue(out.series) == 'Monthly' & tclvalue(file.period) == 'Monthly data')){
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
		tkconfigure(en.minfrac, state = stateo0b)
		tkconfigure(cb.opfun, state = stateo1)
		tkconfigure(en.opthres, state = stateo2)
	})

	tkbind(cb.startSeas, "<<ComboboxSelected>>", function(){
		if(tclvalue(out.series) == '3-Months') len <- 2
		if(tclvalue(out.series) == '6-Months') len <- 5
		if(tclvalue(out.series) == 'Annual') len <- 11
		mon1 <- which(MOIS%in%str_trim(tclvalue(start.seas)))
		mon1 <- (mon1 + len)%%12
		mon1[mon1 == 0] <- 12
		tclvalue(end.seas) <- MOIS[mon1]

		tclvalue(startMonth) <- tclvalue(start.seas)
		tclvalue(endMonth) <- tclvalue(end.seas)
	})

	#############################

	frameAggr <- ttklabelframe(subfr2, text = "Time series aggregation", relief = 'groove')

	aggr.fun <- tclVar(GeneralParameters$aggr.series$aggr.fun)
	min.frac <- tclVar(GeneralParameters$aggr.series$min.frac)
	opr.fun <- tclVar(GeneralParameters$aggr.series$opr.fun)
	opr.thres <- tclVar(GeneralParameters$aggr.series$opr.thres)

	AGGREGFUN <- c("mean", "sum", "count")
	if(GeneralParameters$in.series == 'daily' & GeneralParameters$out.series$out.series != 'daily') AGGREGFUN <- AGGREGFUN[-3]
	
	if(GeneralParameters$in.series == GeneralParameters$out.series$out.series){
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

	txt.aggfun <- tklabel(frameAggr, text = 'Function', anchor = 'w', justify = 'left')
	cb.aggfun <- ttkcombobox(frameAggr, values = AGGREGFUN, textvariable = aggr.fun, width = 6, state = stateo0a)
	txt.minfrac <- tklabel(frameAggr, text = 'Min.Frac', anchor = 'w', justify = 'left')
	en.minfrac <- tkentry(frameAggr, textvariable = min.frac, width = 6, state = stateo0b)
	txt.opfun <- tklabel(frameAggr, text = 'Operator', anchor = 'w', justify = 'left')
	cb.opfun <- ttkcombobox(frameAggr, values = c(">=", ">", "<=", "<"), textvariable = opr.fun, width = 6, state = stateo1)
	txt.opthres <- tklabel(frameAggr, text = 'Threshold', anchor = 'w', justify = 'left')
	en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = 6, width = 6, state = stateo2)

	tkgrid(txt.aggfun, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.aggfun, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.minfrac, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.minfrac, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.opfun, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.opfun, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.opthres, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.opthres, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.aggfun, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
	status.bar.display(cb.aggfun, TextOutputVar, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
	infobulle(en.minfrac, 'Minimum fraction of available data that must be present within each output time step')
	status.bar.display(en.minfrac, TextOutputVar, 'Minimum fraction of available data that must be present within each output time step')
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

	#############################
	tkgrid(frameDate, row = 0, column = 0, sticky = 'we')
	tkgrid(frameTSOut, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameAggr, row = 2, column = 0, sticky = 'we', pady = 3)

	######################################################################################################

	#Tab3
	frTab3 <- tkframe(cmd.tab3)
	tkgrid(frTab3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab3, 0, weight = 1)

	scrw3 <- bwScrolledWindow(frTab3)
	tkgrid(scrw3)
	tkgrid.columnconfigure(scrw3, 0, weight = 1)
	subfr3 <- bwScrollableFrame(scrw3, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr3, 0, weight = 1)

	##########################################

	frameZoom <- ttklabelframe(subfr3, text = "Zoom", relief = 'groove')

	xentr1.zoom <- tkentry(frameZoom, width = 7, justify = "left", textvariable = EnvZoomPars$xx1)
	xentr2.zoom <- tkentry(frameZoom, width = 7, justify = "left", textvariable = EnvZoomPars$xx2)
	yentr1.zoom <- tkentry(frameZoom, width = 7, justify = "left", textvariable = EnvZoomPars$yy1)
	yentr2.zoom <- tkentry(frameZoom, width = 7, justify = "left", textvariable = EnvZoomPars$yy2)
	bt.centre.zoom <- tklabel(frameZoom, image = pikCentre)

	EnvZoomPars$btZoomP <- tkbutton(frameZoom, image = pikZoomPlus, relief = 'raised', bg = 'lightblue', state = 'normal')
	EnvZoomPars$btZoomM <- tkbutton(frameZoom, image = pikZoomMinus, relief = 'raised', bg = 'lightblue', state = 'normal')
	EnvZoomPars$btZoomRect <- tkbutton(frameZoom, image = pikZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
	EnvZoomPars$btPanImg <- tkbutton(frameZoom, image = pikPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
	EnvZoomPars$btRedraw <- tkbutton(frameZoom, image = pikRedraw, relief = 'raised', bg = 'lightblue')
	EnvZoomPars$btReset <- tkbutton(frameZoom, image = pikReset, relief = 'raised')

	#################
	tkconfigure(EnvZoomPars$btRedraw, command = function(){
		ZoomXYval <<- as.numeric(c(tclvalue(EnvZoomPars$xx1), tclvalue(EnvZoomPars$xx2),
								  tclvalue(EnvZoomPars$yy1), tclvalue(EnvZoomPars$yy2)))

		# ZoomXYval
		tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))+1
		if(length(AllOpenTabType) > 0){
			if(AllOpenTabType[[tabid]] == "img" & !is.null(notebookTab)){
				if(AllOpenTabData[[tabid]][[1]][[1]]$ID  == notebookTab[[2]]){
					assign("ZoomXYval", ZoomXYval, envir = environment(AllOpenTabData[[tabid]][[2]][[2]]$fun))
					refreshPlot1(W = AllOpenTabData[[tabid]][[2]][[1]],
								img = AllOpenTabData[[tabid]][[2]][[2]],
								hscale = as.numeric(tclvalue(tkget(spinH))),
								vscale = as.numeric(tclvalue(tkget(spinV))))
					tkconfigure(EnvZoomPars$btRedraw, relief = 'raised', bg = 'lightblue')
				}
			}
		}
	})

	tkconfigure(EnvZoomPars$btReset, command = function(){
		ZoomXYval <<- ZoomXYval0
		tclvalue(EnvZoomPars$xx1) <- ZoomXYval0[1]
		tclvalue(EnvZoomPars$xx2) <- ZoomXYval0[2]
		tclvalue(EnvZoomPars$yy1) <- ZoomXYval0[3]
		tclvalue(EnvZoomPars$yy2) <- ZoomXYval0[4]

		# ZoomXYval
		tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))+1
		if(length(AllOpenTabType) > 0){
			if(AllOpenTabType[[tabid]] == "img" & !is.null(notebookTab)){
				if(AllOpenTabData[[tabid]][[1]][[1]]$ID  == notebookTab[[2]]){
					assign("ZoomXYval", ZoomXYval, envir = environment(AllOpenTabData[[tabid]][[2]][[2]]$fun))
					refreshPlot1(W = AllOpenTabData[[tabid]][[2]][[1]],
								img = AllOpenTabData[[tabid]][[2]][[2]],
								hscale = as.numeric(tclvalue(tkget(spinH))),
								vscale = as.numeric(tclvalue(tkget(spinV))))
					tkconfigure(EnvZoomPars$btRedraw, relief = 'raised', bg = 'lightblue')
				}
			}
		}
	})

	tkgrid(xentr1.zoom, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(xentr2.zoom, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(yentr1.zoom, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(yentr2.zoom, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(bt.centre.zoom, row = 1, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1)

	tkgrid(EnvZoomPars$btReset, row = 0, column = 3, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(EnvZoomPars$btRedraw, row = 1, column = 3, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(EnvZoomPars$btPanImg, row = 2, column = 3, sticky = 'nswe', rowspan = 1, columnspan = 1)

	tkgrid(EnvZoomPars$btZoomP, row = 0, column = 4, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(EnvZoomPars$btZoomM, row = 1, column = 4, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(EnvZoomPars$btZoomRect, row = 2, column = 4, sticky = 'nswe', rowspan = 1, columnspan = 1)

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

	##########################################

	initializeButZoom <- function(){
			initXYval0 <<- str_trim(c(tclvalue(EnvZoomPars$xx1), tclvalue(EnvZoomPars$xx2),
									tclvalue(EnvZoomPars$yy1), tclvalue(EnvZoomPars$yy2)))

			tclvalue(EnvZoomPars$pressButP) <- 0
			tclvalue(EnvZoomPars$pressButM) <- 0
			tclvalue(EnvZoomPars$pressButRect) <- 0
			tclvalue(EnvZoomPars$pressButDrag) <- 0

			tclvalue(EnvExtractData$pressGetCoords) <- 0

			tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(EnvExtractData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
			stateADD <- if(tclvalue(EnvExtractData$type.extract)%in%c('Multiple Points', 'Multiple Polygons')) "normal" else "disabled"
			tkconfigure(EnvExtractData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
	}

	activateButRedraw <- function(){
		initXYval1 <- str_trim(c(tclvalue(EnvZoomPars$xx1), tclvalue(EnvZoomPars$xx2),
								tclvalue(EnvZoomPars$yy1), tclvalue(EnvZoomPars$yy2)))
		if(!all(initXYval0 == initXYval1)) tkconfigure(EnvZoomPars$btRedraw, relief = 'raised', bg = 'red')
	}

	#################
	tkbind(xentr1.zoom, "<FocusIn>", initializeButZoom)
	tkbind(xentr1.zoom, "<FocusOut>", activateButRedraw)

	tkbind(xentr2.zoom, "<FocusIn>", initializeButZoom)
	tkbind(xentr2.zoom, "<FocusOut>", activateButRedraw)

	tkbind(yentr1.zoom, "<FocusIn>", initializeButZoom)
	tkbind(yentr1.zoom, "<FocusOut>", activateButRedraw)

	tkbind(yentr2.zoom, "<FocusIn>", initializeButZoom)
	tkbind(yentr2.zoom, "<FocusOut>", activateButRedraw)

	####
	tkbind(EnvZoomPars$btRedraw, "<Button-1>", function(){
		tclvalue(EnvZoomPars$pressButP) <- 0
		tclvalue(EnvZoomPars$pressButM) <- 0
		tclvalue(EnvZoomPars$pressButRect) <- 0
		tclvalue(EnvZoomPars$pressButDrag) <- 0

		tclvalue(EnvExtractData$pressGetCoords) <- 0

		tkconfigure(EnvZoomPars$btRedraw, relief = 'raised', bg = 'lightblue')

		tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

		tkconfigure(EnvExtractData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
		stateADD <- if(tclvalue(EnvExtractData$type.extract)%in%c('Multiple Points', 'Multiple Polygons')) "normal" else "disabled"
		tkconfigure(EnvExtractData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
	})

	tkbind(EnvZoomPars$btReset, "<Button-1>", function(){
		tclvalue(EnvZoomPars$pressButP) <- 0
		tclvalue(EnvZoomPars$pressButM) <- 0
		tclvalue(EnvZoomPars$pressButRect) <- 0
		tclvalue(EnvZoomPars$pressButDrag) <- 0

		tclvalue(EnvExtractData$pressGetCoords) <- 0

		tkconfigure(EnvZoomPars$btRedraw, relief = 'raised', bg = 'lightblue')

		tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

		tkconfigure(EnvExtractData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
		stateADD <- if(tclvalue(EnvExtractData$type.extract)%in%c('Multiple Points', 'Multiple Polygons')) "normal" else "disabled"
		tkconfigure(EnvExtractData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
	})

	tkbind(EnvZoomPars$btZoomP, "<Button-1>", function(){
		tclvalue(EnvZoomPars$pressButP) <- 1
		tclvalue(EnvZoomPars$pressButM) <- 0
		tclvalue(EnvZoomPars$pressButRect) <- 0
		tclvalue(EnvZoomPars$pressButDrag) <- 0

		tclvalue(EnvExtractData$pressGetCoords) <- 0

		tkconfigure(EnvZoomPars$btRedraw, relief = 'raised', bg = 'lightblue')

		tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'red', state = 'disabled')
		tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

		tkconfigure(EnvExtractData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
		stateADD <- if(tclvalue(EnvExtractData$type.extract)%in%c('Multiple Points', 'Multiple Polygons')) "normal" else "disabled"
		tkconfigure(EnvExtractData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
	})

	tkbind(EnvZoomPars$btZoomM, "<Button-1>", function(){
		tclvalue(EnvZoomPars$pressButP) <- 0
		tclvalue(EnvZoomPars$pressButM) <- 1
		tclvalue(EnvZoomPars$pressButRect) <- 0
		tclvalue(EnvZoomPars$pressButDrag) <- 0

		tclvalue(EnvExtractData$pressGetCoords) <- 0

		tkconfigure(EnvZoomPars$btRedraw, relief = 'raised', bg = 'lightblue')

		tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'red', state = 'disabled')
		tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

		tkconfigure(EnvExtractData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
		stateADD <- if(tclvalue(EnvExtractData$type.extract)%in%c('Multiple Points', 'Multiple Polygons')) "normal" else "disabled"
		tkconfigure(EnvExtractData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
	})

	tkbind(EnvZoomPars$btZoomRect, "<Button-1>", function(){
		tclvalue(EnvZoomPars$pressButP) <- 0
		tclvalue(EnvZoomPars$pressButM) <- 0
		tclvalue(EnvZoomPars$pressButRect) <- 1
		tclvalue(EnvZoomPars$pressButDrag) <- 0

		tclvalue(EnvExtractData$pressGetCoords) <- 0

		tkconfigure(EnvZoomPars$btRedraw, relief = 'raised', bg = 'lightblue')

		tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'red', state = 'disabled')
		tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

		tkconfigure(EnvExtractData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
		stateADD <- if(tclvalue(EnvExtractData$type.extract)%in%c('Multiple Points', 'Multiple Polygons')) "normal" else "disabled"
		tkconfigure(EnvExtractData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
	})

	tkbind(EnvZoomPars$btPanImg, "<Button-1>", function(){
		tclvalue(EnvZoomPars$pressButP) <- 0
		tclvalue(EnvZoomPars$pressButM) <- 0
		tclvalue(EnvZoomPars$pressButRect) <- 0
		tclvalue(EnvZoomPars$pressButDrag) <- 1

		tclvalue(EnvExtractData$pressGetCoords) <- 0

		tkconfigure(EnvZoomPars$btRedraw, relief = 'raised', bg = 'lightblue')

		tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'red', state = 'disabled')

		tkconfigure(EnvExtractData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
		stateADD <- if(tclvalue(EnvExtractData$type.extract)%in%c('Multiple Points', 'Multiple Polygons')) "normal" else "disabled"
		tkconfigure(EnvExtractData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
	})

	#############################

	frameExtract <- ttklabelframe(subfr3, text = "Extraction Type", relief = 'groove')

	typeEXTRACT <- c('Point', 'Multiple Points', 'Rectangle', 'Polygon', 'Multiple Polygons')
	EnvExtractData$type.extract <- tclVar()
	tclvalue(EnvExtractData$type.extract) <- switch(GeneralParameters$type.extract, 
													'point' = typeEXTRACT[1], 
													'mpoint' = typeEXTRACT[2],
													'rect' = typeEXTRACT[3], 
													'poly' = typeEXTRACT[4],
													'mpoly' = typeEXTRACT[5])
	EnvExtractData$minlonRect <- tclVar()
	EnvExtractData$maxlonRect <- tclVar()
	EnvExtractData$minlatRect <- tclVar()
	EnvExtractData$maxlatRect <- tclVar()
	EnvExtractData$namePoly <- tclVar()

	padLon <- tclVar('0.0')
	padLat <- tclVar('0.0')

	pointrect <- tclVar('Point')
	minrect <- tclVar()
	maxrect <- tclVar()

	statePts <- if(GeneralParameters$type.extract %in% c('point', 'mpoint')) "normal" else "disabled"
	stateRct <- if(GeneralParameters$type.extract == 'rect') "normal" else "disabled"
	statePad <- "normal"
	statePol <- if(GeneralParameters$type.extract %in% c('poly', 'mpoly')) "normal" else "disabled"
	stateADD <- if(GeneralParameters$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"

	txt.Type <- tklabel(frameExtract, text = "Type", anchor = 'e', justify = 'right')
	cb.TypeExtr <- ttkcombobox(frameExtract, values = typeEXTRACT, textvariable = EnvExtractData$type.extract, width = largeur4)

	txt.PointRect <- tklabel(frameExtract, text = tclvalue(pointrect), textvariable = pointrect, anchor = 'e', justify = 'right', bg = 'green')
	txt.MIN <- tklabel(frameExtract, text = tclvalue(minrect), textvariable = minrect)
	txt.MAX <- tklabel(frameExtract, text = tclvalue(maxrect), textvariable = maxrect)
	txt.PAD <- tklabel(frameExtract, text = "Pad")

	txt.LON <- tklabel(frameExtract, text = 'Longitude', anchor = 'e', justify = 'right')
	en.minlon <- tkentry(frameExtract, width = 7, textvariable = EnvExtractData$minlonRect, justify = "left", state = statePts)
	en.maxlon <- tkentry(frameExtract, width = 7, textvariable = EnvExtractData$maxlonRect, justify = "left", state = stateRct)
	txt.PlusM1 <- tklabel(frameExtract, text = '\u00B1')
	en.PadLon <- tkentry(frameExtract, width = 4, textvariable = padLon, justify = "left", state = statePad)

	txt.LAT <- tklabel(frameExtract, text = 'Latitude', anchor = 'e', justify = 'right')
	en.minlat <- tkentry(frameExtract, width = 7, textvariable = EnvExtractData$minlatRect, justify = "left", state = statePts)
	en.maxlat <- tkentry(frameExtract, width = 7, textvariable = EnvExtractData$maxlatRect, justify = "left", state = stateRct)
	txt.PlusM2 <- tklabel(frameExtract, text = '\u00B1')
	en.PadLat <- tkentry(frameExtract, width = 4, textvariable = padLat, justify = "left", state = statePad)

	txt.Polygon <- tklabel(frameExtract, text = "Polygon", anchor = 'e', justify = 'right', bg = 'green')
	cb.Polygon <- ttkcombobox(frameExtract, values = '', textvariable = EnvExtractData$namePoly, state = statePol, width = largeur4)

	EnvExtractData$bt.ADDObj <- tkbutton(frameExtract, text = "ADD", relief = 'raised', bg = 'lightblue', state = stateADD)
	EnvExtractData$bt.GETArea <- tkbutton(frameExtract, text = "GET", relief = 'raised', bg = 'lightblue', state = 'normal')


	#################
	nbpts <- 1
	retMultiP <- NULL

	tkconfigure(EnvExtractData$bt.ADDObj, command = function(){
		if(EnvExtractData$dlgBoxOpen){
			if(tclvalue(EnvExtractData$type.extract) == 'Multiple Points'){			
				tkinsert(retMultiP$textObj, "end", paste(paste('Pts', nbpts, sep = ''),
													tclvalue(EnvExtractData$minlonRect),
													tclvalue(EnvExtractData$minlatRect), "\n"))
				nbpts <<- nbpts+1
			}
			if(tclvalue(EnvExtractData$type.extract) == 'Multiple Polygons'){
				tkinsert(retMultiP$textObj, "end", paste(tclvalue(EnvExtractData$namePoly), "\n"))
			}
		}
		stateADD <- if(tclvalue(EnvExtractData$type.extract)%in%c('Multiple Points', 'Multiple Polygons')) "normal" else "disabled"
		tkconfigure(EnvExtractData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
	})

	tkgrid(txt.Type, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(cb.TypeExtr, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	tkgrid(txt.PointRect, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.MIN, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.MAX, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.PAD, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.LON, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.minlon, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.maxlon, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.PlusM1, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.PadLon, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.LAT, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.minlat, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.maxlat, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.PlusM2, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.PadLat, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.Polygon, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(cb.Polygon, row = 4, column = 1, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	tkgrid(EnvExtractData$bt.ADDObj, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(EnvExtractData$bt.GETArea, row = 5, column = 2, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.PadLon, 'Add value in decimal degree to get\nrectangle centered at the target points')
	status.bar.display(en.PadLon, TextOutputVar, 'Add value in decimal degree to get\nrectangle centered at the target points')
	infobulle(en.PadLat, 'Add value in decimal degree to get rectangle\ncentered at the target points')
	status.bar.display(en.PadLat, TextOutputVar, 'Add value in decimal degree to get\nrectangle centered at the target points')
	infobulle(EnvExtractData$bt.GETArea, 'Before to click on the map to select object, click here to activate the function')
	status.bar.display(EnvExtractData$bt.GETArea, TextOutputVar, 'Before to click on the map to select object, click here to activate the function')
	infobulle(EnvExtractData$bt.ADDObj, 'Multiple Points&Polygons: after selecting object from the map\nclick here to add the object to the list')
	status.bar.display(EnvExtractData$bt.ADDObj, TextOutputVar, 'Multiple Points&Polygons: after selecting object from the map\nclick here to add the object to the list')

	#################
	tkbind(cb.Polygon, "<<ComboboxSelected>>", function(){
		if(tclvalue(EnvExtractData$namePoly) != ''){
			shpfopen <- getShpOpenData(shpFile)
			shpf <- shpfopen[[2]]
			ids <- as.numeric(tclvalue(tcl(EnvExtractData$cb.shpAttr, 'current')))+1
			selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(EnvExtractData$namePoly), ])
		}else{
			selectedPolygon <- NULL
		}

		# selectedPolygon
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

	tkbind(EnvExtractData$bt.GETArea, "<Button-1>", function(){
		tclvalue(EnvZoomPars$pressButP) <- 0
		tclvalue(EnvZoomPars$pressButM) <- 0
		tclvalue(EnvZoomPars$pressButRect) <- 0
		tclvalue(EnvZoomPars$pressButDrag) <- 0

		tclvalue(EnvExtractData$pressGetCoords) <- 1

		tkconfigure(EnvZoomPars$btRedraw, relief = 'raised', bg = 'lightblue')

		tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

		tkconfigure(EnvExtractData$bt.GETArea, relief = 'raised', bg = 'red', state = 'disabled')
		stateADD <- if(tclvalue(EnvExtractData$type.extract)%in%c('Multiple Points', 'Multiple Polygons')) "normal" else "disabled"
		tkconfigure(EnvExtractData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
	})



	tkbind(cb.TypeExtr, "<<ComboboxSelected>>", function(){
		selectedPolygon <- NULL
		OutFileFormat <- c('CDT Format', 'CPT Format', 'NetCDF', 'Time|Lat|Lon|Value Format')

		if(tclvalue(EnvExtractData$type.extract) == 'Point'){
			if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)

			statePts <- 'normal'
			stateRct <- 'disabled'
			statePad <- 'normal'
			statePol <- 'disabled'
			stateADD <- 'disabled'

			pointrectVal <- 'Point'
			minrectVal <- ''
			maxrectVal <- ''

			stateSpAv <- 'disabled'
			spatAverg <- '0'
			txtfileORdir <- 'File to save extracted data'
			colfileORdir <- 'lightblue'
			isFile <- TRUE
			OutFileFormat <- OutFileFormat[1:2]
			out.formatVAL <- if(tclvalue(out.format) %in% c('NetCDF', 'Time|Lat|Lon|Value Format')) OutFileFormat[1] else tclvalue(out.format)
		}

		##
		if(tclvalue(EnvExtractData$type.extract) == 'Rectangle'){
			if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)

			statePts <- 'normal'
			stateRct <- 'normal'
			statePad <- 'disabled'
			statePol <- 'disabled'
			stateADD <- 'disabled'

			pointrectVal <- 'Rectangle'
			minrectVal <- 'Min'
			maxrectVal <- 'Max'

			stateSpAv <- if(tclvalue(out.format) == 'CPT Format') 'normal' else 'disabled'
			if(tclvalue(out.format) %in% c('NetCDF', 'Time|Lat|Lon|Value Format')){
				spatAverg <- "0"
			}else if(tclvalue(out.format) == 'CDT Format'){
				spatAverg <- "1"
			}else spatAverg <- tclvalue(spatAverage)

			if(tclvalue(out.format) == 'NetCDF'){
				txtfileORdir <- 'Directory to save extracted data'
				colfileORdir <- 'lightgreen'
				isFile <- FALSE
			}else{
				txtfileORdir <- 'File to save extracted data'
				colfileORdir <- 'lightblue'
				isFile <- TRUE
			}

			out.formatVAL <- tclvalue(out.format)
		}

		##
		if(tclvalue(EnvExtractData$type.extract) == 'Polygon'){
			if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)

			statePts <- 'disabled'
			stateRct <- 'disabled'
			statePad <- 'disabled'
			statePol <- 'normal'
			stateADD <- 'disabled'

			pointrectVal <- tclvalue(pointrect)
			minrectVal <- ''
			maxrectVal <- ''

			stateSpAv <- if(tclvalue(out.format) == 'CPT Format') 'normal' else 'disabled'
			if(tclvalue(out.format) %in% c('NetCDF', 'Time|Lat|Lon|Value Format')){
				spatAverg <- "0"
			}else if(tclvalue(out.format) == 'CDT Format'){
				spatAverg <- "1"
			}else spatAverg <- tclvalue(spatAverage)

			if(tclvalue(out.format) == 'NetCDF'){
				txtfileORdir <- 'Directory to save extracted data'
				colfileORdir <- 'lightgreen'
				isFile <- FALSE
			}else{
				txtfileORdir <- 'File to save extracted data'
				colfileORdir <- 'lightblue'
				isFile <- TRUE
			}

			out.formatVAL <- tclvalue(out.format)

			if(tclvalue(EnvExtractData$namePoly) != ''){
				shpfopen <- getShpOpenData(shpFile)
				shpf <- shpfopen[[2]]
				ids <- as.numeric(tclvalue(tcl(EnvExtractData$cb.shpAttr, 'current')))+1
				selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(EnvExtractData$namePoly), ])
			}
		}

		##
		if(tclvalue(EnvExtractData$type.extract) == 'Multiple Points'){
			if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)
			retMultiP <<- extractTS.previewWin(main.win, c('normal', 'disabled'),
							list(EnvExtractData$cb.shpAttr, shpFile), "mpoint")

			statePts <- 'normal'
			stateRct <- 'disabled'
			statePad <- 'normal'
			statePol <- 'disabled'
			stateADD <- 'normal'

			pointrectVal <- 'Point'
			minrectVal <- ''
			maxrectVal <- ''

			stateSpAv <- 'disabled'
			spatAverg <- '0'
			txtfileORdir <- 'File to save extracted data'
			colfileORdir <- 'lightblue'
			isFile <- TRUE
			OutFileFormat <- OutFileFormat[1:2]
			out.formatVAL <- if(tclvalue(out.format) %in% c('NetCDF', 'Time|Lat|Lon|Value Format')) OutFileFormat[1] else tclvalue(out.format)
		}

		##
		if(tclvalue(EnvExtractData$type.extract) == 'Multiple Polygons'){
			if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)
			retMultiP <<- extractTS.previewWin(main.win, c('disabled', 'normal'),
							list(EnvExtractData$cb.shpAttr, shpFile), "mpoly")

			statePts <- 'disabled'
			stateRct <- 'disabled'
			statePad <- 'disabled'
			statePol <- 'normal'
			stateADD <- 'normal'

			pointrectVal <- tclvalue(pointrect)
			minrectVal <- ''
			maxrectVal <- ''

			stateSpAv <- 'disabled'
			spatAverg <- '0'
			txtfileORdir <- 'File to save extracted data'
			colfileORdir <- 'lightblue'
			isFile <- TRUE
			OutFileFormat <- OutFileFormat[1:2]
			out.formatVAL <- if(tclvalue(out.format) %in% c('NetCDF', 'Time|Lat|Lon|Value Format')) OutFileFormat[1] else tclvalue(out.format)

			if(tclvalue(EnvExtractData$namePoly) != ''){
				shpfopen <- getShpOpenData(shpFile)
				shpf <- shpfopen[[2]]
				ids <- as.numeric(tclvalue(tcl(EnvExtractData$cb.shpAttr, 'current')))+1
				selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(EnvExtractData$namePoly), ])
			}
		}

		tkconfigure(en.minlon, state = statePts)
		tkconfigure(en.minlat, state = statePts)
		tkconfigure(en.maxlon, state = stateRct)
		tkconfigure(en.maxlat, state = stateRct)
		tkconfigure(en.PadLon, state = statePad)
		tkconfigure(en.PadLat, state = statePad)

		tkconfigure(cb.Polygon, state = statePol)
		tkconfigure(EnvExtractData$bt.ADDObj, state = stateADD)

		tclvalue(pointrect) <- pointrectVal
		tclvalue(minrect) <- minrectVal
		tclvalue(maxrect) <- maxrectVal

		##
		tkconfigure(chk.SpAvrg, state = stateSpAv)
		tclvalue(spatAverage) <- spatAverg
		tkconfigure(txt.saveData, bg = colfileORdir)
		tclvalue(fileORdir)  <- txtfileORdir
		tkconfigure(bt.saveData, command = function() fileORdir2Save(file2save, isFile = isFile))

		tkconfigure(cb.outFormat, values = OutFileFormat)
		tclvalue(out.format) <- out.formatVAL

		##
		tclvalue(EnvExtractData$minlonRect) <- ''
		tclvalue(EnvExtractData$maxlonRect) <- ''
		tclvalue(EnvExtractData$minlatRect) <- ''
		tclvalue(EnvExtractData$maxlatRect) <- ''
		tkconfigure(EnvExtractData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')

		# selectedPolygon
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

	#############################
	tkgrid(frameZoom, row = 0, column = 0, sticky = 'we')
	tkgrid(frameExtract, row = 1, column = 0, sticky = 'we', pady = 3)

	######################################################################################################

	#Tab4
	frTab4 <- tkframe(cmd.tab4)
	tkgrid(frTab4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab4, 0, weight = 1)

	scrw4 <- bwScrolledWindow(frTab4)
	tkgrid(scrw4)
	tkgrid.columnconfigure(scrw4, 0, weight = 1)
	subfr4 <- bwScrollableFrame(scrw4, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr4, 0, weight = 1)

	##########################################

	frameOutTypeS <- ttklabelframe(subfr4, text = "Output Time series", relief = 'groove')

	OUTTypeSeries <- c('Raw Time Series', 'Anomalies', 'Standardized Anomalies', 'Climatologies')
	type.series <- tclVar()
	tclvalue(type.series) <- switch(GeneralParameters$type.series, 
										'rawts' = OUTTypeSeries[1],
										'anom' = OUTTypeSeries[2],
										'stanom' = OUTTypeSeries[3],
										'climato' = OUTTypeSeries[4])
	UseAllYears <- tclVar(GeneralParameters$climato$all.years)
	climYear1 <- tclVar(GeneralParameters$climato$start.year)
	climYear2 <- tclVar(GeneralParameters$climato$end.year)
	minYear <- tclVar(GeneralParameters$climato$min.year)
	winsize <- tclVar(GeneralParameters$climato$winsize)

	stateClim0 <- if(GeneralParameters$type.series == 'rawts') 'disabled' else 'normal'
	stateClim1 <- if(GeneralParameters$type.series != 'rawts' & !GeneralParameters$climato$all.years) 'normal' else 'disabled'
	stateClim2 <- if(GeneralParameters$in.series == 'daily' & GeneralParameters$type.series != 'rawts') "normal" else "disabled"

	cb.typeseries <- ttkcombobox(frameOutTypeS, values = OUTTypeSeries, textvariable = type.series, width = largeur1)
	chk.allYears <- tkcheckbutton(frameOutTypeS, variable = UseAllYears, text =  "Use all years from the input data", anchor = 'w', justify = 'left', state = stateClim0)
	txt.startYear <- tklabel(frameOutTypeS, text = "Start Year",  anchor = 'e', justify = 'right')
	en.startYear <- tkentry(frameOutTypeS, textvariable = climYear1, width = 6, state = stateClim1)
	txt.endYear <- tklabel(frameOutTypeS, text = "End Year",  anchor = 'e', justify = 'right')
	en.endYear <- tkentry(frameOutTypeS, textvariable = climYear2, width = 6, state = stateClim1)
	txt.minYear <- tklabel(frameOutTypeS, text = "Min.Nb Year",  anchor = 'e', justify = 'right')
	en.minYear <- tkentry(frameOutTypeS, textvariable = minYear, width = 6, state = stateClim0)
	txt.winsize <- tklabel(frameOutTypeS, text = "Winsize",  anchor = 'e', justify = 'right')
	en.winsize <- tkentry(frameOutTypeS, textvariable = winsize, width = 6, state = stateClim2)

	tkgrid(cb.typeseries, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.allYears, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.startYear, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.startYear, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.endYear, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.endYear, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.minYear, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.minYear, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.winsize, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.winsize, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.typeseries, 'Select the type of output data')
	status.bar.display(cb.typeseries, TextOutputVar, 'Select the type of output data')
	infobulle(chk.allYears, "Check this box to use all years from the input data to calculate the climatology")
	status.bar.display(chk.allYears, TextOutputVar, "Check this box to use all years from the input data to calculate the climatology")
	infobulle(en.startYear, "Enter the start year of the period in which the climatology will be calculated")
	status.bar.display(en.startYear, TextOutputVar, "Enter the start year of the period in which the climatology will be calculated")
	infobulle(en.endYear, "Enter the end year of the period in which the climatology will be calculated")
	status.bar.display(en.endYear, TextOutputVar, "Enter the end year of the period in which the climatology will be calculated")
	infobulle(en.minYear, "Minimum number of year without missing values required to calculate climatology")
	status.bar.display(en.minYear, TextOutputVar, "Minimum number of year without missing values required to calculate climatology")
	infobulle(en.winsize, "Use sliding window to compute daily climatology,\ndata will be average within this window size")
	status.bar.display(en.winsize, TextOutputVar, "Use sliding window to compute daily climatology,\ndata will be average within this window size")

	#################
	tkbind(cb.typeseries, "<<ComboboxSelected>>", function(){
		if(tclvalue(type.series) != 'Raw Time Series'){
			stateClim0 <- "normal"
			stateClim1 <- if(tclvalue(UseAllYears) == '1') "disabled" else "normal"
			stateClim2 <- if(tclvalue(file.period) == 'Daily data') "normal" else "disabled"
		}else{
			stateClim0 <- "disabled"
			stateClim1 <- "disabled"
			stateClim2 <- "disabled"
		}
		tkconfigure(chk.allYears, state = stateClim0)
		tkconfigure(en.minYear, state = stateClim0)
		tkconfigure(en.startYear, state = stateClim1)
		tkconfigure(en.endYear, state = stateClim1)
		tkconfigure(en.winsize, state = stateClim2)
	})

	tkbind(chk.allYears, "<Button-1>", function(){
		stateClim1 <- if(tclvalue(UseAllYears) == '1') "normal" else "disabled"
		tkconfigure(en.startYear, state = stateClim1)
		tkconfigure(en.endYear, state = stateClim1)
	})

	#############################

	frameOutFormat <- ttklabelframe(subfr4, text = "Output File Format", relief = 'groove')

	OutFileFormat <- c('CDT Format', 'CPT Format', 'NetCDF', 'Time|Lat|Lon|Value Format')
	if(GeneralParameters$type.extract %in% c("point", "mpoint", "mpoly")) OutFileFormat <- OutFileFormat[1:2]
	out.format <- tclVar()
	tclvalue(out.format) <- switch(GeneralParameters$out.data$format, 
									'cdt' = OutFileFormat[1], 
									'cpt' = OutFileFormat[1], 
									'ncdf' = OutFileFormat[2],
									'tyxz' = OutFileFormat[3])


	cb.outFormat <- ttkcombobox(frameOutFormat, values = OutFileFormat, textvariable = out.format, width = largeur1)

	tkgrid(cb.outFormat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.outFormat, 'Select the format of the output data')
	status.bar.display(cb.outFormat, TextOutputVar, 'Select the format of the output data')

	#################
	tkbind(cb.outFormat, "<<ComboboxSelected>>", function(){
		if(tclvalue(EnvExtractData$type.extract) %in% c('Point', 'Multiple Points', 'Multiple Polygons')){
			txtfileORdir <- 'File to save extracted data'
			colfileORdir <- 'lightblue'
			isFile <- TRUE

			spatAverg <- "0"
			stateSpAv <- 'disabled'
		}else{
			if(tclvalue(out.format) == 'NetCDF'){
				txtfileORdir <- 'Directory to save extracted data'
				colfileORdir <- 'lightgreen'
				isFile <- FALSE
			}else{
				txtfileORdir <- 'File to save extracted data'
				colfileORdir <- 'lightblue'
				isFile <- TRUE
			}

			if(tclvalue(out.format) %in% c('NetCDF', 'Time|Lat|Lon|Value Format')){
				spatAverg <- "0"
			}else if(tclvalue(out.format) == 'CDT Format'){
				spatAverg <- "1"
			}else spatAverg <- tclvalue(spatAverage)

			stateSpAv <- if(tclvalue(out.format) == 'CPT Format') 'normal' else 'disabled'
		}

		tkconfigure(chk.SpAvrg, state = stateSpAv)
		tclvalue(spatAverage) <- spatAverg

		tkconfigure(txt.saveData, bg = colfileORdir)
		tclvalue(fileORdir)  <- txtfileORdir
		tkconfigure(bt.saveData, command = function() fileORdir2Save(file2save, isFile = isFile))
	})

	#############################
	frameSpAvrg <- tkframe(subfr4, relief = 'groove', borderwidth = 2)

	spatAverage <- tclVar(GeneralParameters$out.data$sp.avrg)
	stateSpAv <- if(GeneralParameters$type.extract %in% c("point", "mpoint", "mpoly")) 'disabled' else 'normal'

	txt.SpAvrg <- tklabel(frameSpAvrg, text = 'Saptially Average Over Selected Area', anchor = 'e', justify = 'right')
	chk.SpAvrg <- tkcheckbutton(frameSpAvrg, variable = spatAverage, state = stateSpAv)

	tkgrid(txt.SpAvrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(chk.SpAvrg, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#############################
	frameSave <- tkframe(subfr4, relief = 'groove', borderwidth = 2)

	if(GeneralParameters$out.data$sp.avrg){
		txtfileORdir <- 'File to save extracted data'
		colfileORdir <- 'lightblue'
		isFile <- TRUE
	}else{
		if((GeneralParameters$type.extract %in% c("rect", "poly")) &
			GeneralParameters$out.data$format == "ncdf"){
			txtfileORdir <- 'Directory to save extracted data'
			colfileORdir <- 'lightgreen'
			isFile <- FALSE
		}else{
			txtfileORdir <- 'File to save extracted data'
			colfileORdir <- 'lightblue'
			isFile <- TRUE
		}
	}

	fileORdir <- tclVar(txtfileORdir)
	file2save <- tclVar(GeneralParameters$out.data$outdir)

	txt.saveData <- tklabel(frameSave, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left', bg = colfileORdir)
	en.saveData <- tkentry(frameSave, textvariable = file2save, width = largeur5)
	bt.saveData <- tkbutton(frameSave, text = "...")

	tkconfigure(bt.saveData, command = function() fileORdir2Save(file2save, isFile = isFile))

	tkgrid(txt.saveData, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.saveData, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.saveData, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.saveData, 'Enter the full path to directory to save extracted data')
	status.bar.display(en.saveData, TextOutputVar, 'Enter the full path to directory to save extracted data')
	infobulle(bt.saveData, 'or browse here')
	status.bar.display(bt.saveData, TextOutputVar, 'or browse here')

	#############################

	bt.Extract.Data <- ttkbutton(subfr4, text = "EXTRACT DATA")

	tkconfigure(bt.Extract.Data, command = function(){
		GeneralParameters$in.series <- switch(tclvalue(file.period),
		 									'Daily data' = 'daily',
											'Dekadal data' =  'dekadal',
											'Monthly data' = 'monthly')
		GeneralParameters$ncdf.file$dir <- str_trim(tclvalue(ncDIR))
		GeneralParameters$ncdf.file$sample <- str_trim(tclvalue(ncSample))
		GeneralParameters$ncdf.file$format <- str_trim(tclvalue(ncFormat))

		GeneralParameters$shp.file$shp <- str_trim(tclvalue(shpFile))
		GeneralParameters$shp.file$attr <- str_trim(tclvalue(shpAttr))

		GeneralParameters$date.range$start.year <- as.numeric(str_trim(tclvalue(istart.yrs)))
		GeneralParameters$date.range$start.mon <- as.numeric(str_trim(tclvalue(istart.mon)))
		GeneralParameters$date.range$start.day <- as.numeric(str_trim(tclvalue(istart.day)))
		GeneralParameters$date.range$end.year <- as.numeric(str_trim(tclvalue(iend.yrs)))
		GeneralParameters$date.range$end.mon <- as.numeric(str_trim(tclvalue(iend.mon)))
		GeneralParameters$date.range$end.day <- as.numeric(str_trim(tclvalue(iend.day)))

		GeneralParameters$date.range$start.month <- which(MOIS%in%str_trim(tclvalue(startMonth)))
		GeneralParameters$date.range$end.month <- which(MOIS%in%str_trim(tclvalue(endMonth)))

		GeneralParameters$out.series$out.series <- switch(tclvalue(out.series),
														'Daily' = 'daily',
														'Dekadal' = 'dekadal',
														'Monthly' = 'monthly',
														'3-Months' = 'seasonal3', 
														'6-Months' = 'seasonal6', 
														'Annual' = 'annual')

		GeneralParameters$out.series$start.seas <- which(MOIS%in%str_trim(tclvalue(start.seas)))
		GeneralParameters$out.series$end.seas <- which(MOIS%in%str_trim(tclvalue(end.seas)))

		GeneralParameters$aggr.series$aggr.fun <- str_trim(tclvalue(aggr.fun))
		GeneralParameters$aggr.series$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
		GeneralParameters$aggr.series$opr.fun <- str_trim(tclvalue(opr.fun))
		GeneralParameters$aggr.series$opr.thres <- as.numeric(str_trim(tclvalue(opr.thres)))

		GeneralParameters$type.series <- switch(tclvalue(type.series),
												'Raw Time Series' = 'rawts',
												'Anomalies' = 'anom',
												'Standardized Anomalies' = 'stanom',
												'Climatologies' = 'climato')
		GeneralParameters$climato$all.years <- switch(tclvalue(UseAllYears), '0' = FALSE, '1' = TRUE)
		GeneralParameters$climato$start.year <- as.numeric(str_trim(tclvalue(climYear1)))
		GeneralParameters$climato$end.year <- as.numeric(str_trim(tclvalue(climYear2)))
		GeneralParameters$climato$min.year <- as.numeric(str_trim(tclvalue(minYear)))
		GeneralParameters$climato$winsize <- as.numeric(str_trim(tclvalue(winsize)))

		GeneralParameters$type.extract <- switch(tclvalue(EnvExtractData$type.extract),
												'Point' = 'point', 
												'Multiple Points' = 'mpoint',
												'Rectangle' = 'rect', 
												'Polygon' = 'poly',
												'Multiple Polygons' = 'mpoly')

		GeneralParameters$Geom <- NULL
		GeneralParameters$Geom$minlon <- as.numeric(str_trim(tclvalue(EnvExtractData$minlonRect)))
		GeneralParameters$Geom$maxlon <- as.numeric(str_trim(tclvalue(EnvExtractData$maxlonRect)))
		GeneralParameters$Geom$minlat <- as.numeric(str_trim(tclvalue(EnvExtractData$minlatRect)))
		GeneralParameters$Geom$maxlat <- as.numeric(str_trim(tclvalue(EnvExtractData$maxlatRect)))
		GeneralParameters$Geom$padlon <- as.numeric(str_trim(tclvalue(padLon)))
		GeneralParameters$Geom$padlat <- as.numeric(str_trim(tclvalue(padLat)))
		GeneralParameters$Geom$namePoly <- str_trim(tclvalue(EnvExtractData$namePoly))
		GeneralParameters$Geom$multiObj <- EnvExtractData$multiptspoly

		GeneralParameters$out.data$format <- switch(tclvalue(out.format),
													'CDT Format' = 'cdt', 
													'CPT Format' = 'cpt', 
													'NetCDF' = 'ncdf',
													'Time|Lat|Lon|Value Format' = 'tyxz')
	
		GeneralParameters$out.data$sp.avrg <- switch(tclvalue(spatAverage), '0' = FALSE, '1' = TRUE)
		GeneralParameters$out.data$outdir <- str_trim(tclvalue(file2save))

		# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

		tkconfigure(main.win, cursor = 'watch')
		InsertMessagesTxt(main.txt.out, "Extraction .................")
		tcl('update')
		ret <- tryCatch(
			ExtractDataProcs(GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e){
				 errorFun(e)
			},
			finally = {
				tkconfigure(main.win, cursor = '')
			}
		)

		if(!is.null(ret)){
			if(ret == 0) InsertMessagesTxt(main.txt.out, "Extraction finished successfully")
			else InsertMessagesTxt(main.txt.out, "Extraction failed", format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, "Extraction failed", format = TRUE)
		}
	})

	#############################
	tkgrid(frameOutTypeS, row = 0, column = 0, sticky = 'we')
	tkgrid(frameOutFormat, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameSpAvrg, row = 2, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameSave, row = 3, column = 0, sticky = 'we', pady = 3)
	tkgrid(bt.Extract.Data, row = 4, column = 0, sticky = 'we', pady = 3)

	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)

	######
	return(cmd.frame)
}



ExtractDataPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()

	largeur <- as.integer(w.scale(21)/sfont0)
	wncdf_ff <- as.integer(w.scale(14)/sfont0)

	#scrollable frame width
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(20)
		hscrlwin <- h.scale(31.5)
	}else{
		wscrlwin <- w.scale(24)
		hscrlwin <- h.scale(38.5)
	}

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

	##############
	file.period <- tclVar()
	tclvalue(file.period) <- 'Dekadal data'
	dirNetCDF <- tclVar()
	netCDFff <- tclVar("rr_mrg_%s%s%s.nc")
	Admin_shp <- tclVar()
	Admin_var <- tclVar()

	##############
	xx1 <<- tclVar()
	xx2 <<- tclVar()
	yy1 <<- tclVar()
	yy2 <<- tclVar()
	ZoomXYval0 <- NULL
	notebookTab <- NULL

	#######################

	cbprd.tab1 <- ttkcombobox(subfr1, values = c('Daily data', 'Dekadal data', 'Monthly data'), textvariable = file.period)
	
	sep1.tab1 <- ttkseparator(subfr1)
	lab1.tab1 <- tklabel(subfr1, text = 'Directory of NetCDF files', anchor = 'w', justify = 'left')
	dirCDF.tab1 <- tkentry(subfr1, textvariable = dirNetCDF, width = largeur)
	bdirCDF.tab1 <- tkbutton(subfr1, text = "...")
	cap1.tab1 <- tklabel(subfr1, text = "NetCDF file format", anchor = 'e', justify = 'right')
	netCDFff.tab1 <- tkentry(subfr1, textvariable = netCDFff, width = wncdf_ff)
	
	sep2.tab1 <- ttkseparator(subfr1)
	lab2.tab1 <- tklabel(subfr1, text = "Shapefile for administrative boundaries", anchor = 'w', justify = 'left')
	cbSHP.tab1 <- ttkcombobox(subfr1, values = unlist(listOpenFiles), textvariable = Admin_shp, width = largeur)
	btSHP.tab1 <- tkbutton(subfr1, text = "...")
	lab3.tab1 <- tklabel(subfr1, text = "Attribute field to be used and displayed", anchor = 'w', justify = 'left')
	adminVar.tab1 <<- ttkcombobox(subfr1, values = '', textvariable = Admin_var)
	
	sep3.tab1 <- ttkseparator(subfr1)
	open.tab1 <- tkbutton(subfr1, text = "Open Attribute Table")
	display.tab1 <- tkbutton(subfr1, text = "Display Map")

	##########################################

	tkconfigure(bdirCDF.tab1, command = function(){
		dir4cdf <- tk_choose.dir(tclvalue(dirNetCDF), "")
		tclvalue(dirNetCDF) <- if(is.na(dir4cdf)) "" else dir4cdf
	})

	tkconfigure(btSHP.tab1, command = function(){
		shp.opfiles <- getOpenShp(main.win, all.opfiles)

		if(!is.null(shp.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'shp'
			AllOpenFilesData[[nopf+1]] <<- shp.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(Admin_shp) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cbSHP.tab1, values = unlist(listOpenFiles), textvariable = Admin_shp)

			shpf <- getShpOpenData(Admin_shp)
			shpAttr <- names(shpf[[2]]@data)
			tclvalue(Admin_var) <- shpAttr[1]
			tkconfigure(adminVar.tab1, values = shpAttr, textvariable = Admin_var)
		}
	})

	tkconfigure(open.tab1, command = function(){
		shpf <- getShpOpenData(Admin_shp)
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

	tkconfigure(display.tab1, command = function(){
		shpofile <- getShpOpenData(Admin_shp)
		if(!is.null(shpofile)){
			shpf <- shpofile[[2]]
			lo1 <- round(bbox(shpf)[1, 1], 4)
			lo2 <- round(bbox(shpf)[1, 2], 4)
			la1 <- round(bbox(shpf)[2, 1], 4)
			la2 <- round(bbox(shpf)[2, 2], 4)
			ZoomXYval0 <<- c(lo1, lo2, la1, la2)
			tclvalue(xx1) <<- lo1
			tclvalue(xx2) <<- lo2
			tclvalue(yy1) <<- la1
			tclvalue(yy2) <<- la2
			ZoomXYval <- as.numeric(c(tclvalue(xx1), tclvalue(xx2), tclvalue(yy1), tclvalue(yy2)))

			imgContainer <- displayMap4Extraction(tknotes, shpf, ZoomXYval, notebookTab)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab, AllOpenTabType, AllOpenTabData)
			notebookTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}else InsertMessagesTxt(main.txt.out, 'Provide the ESRI shapfile for for administrative boundaries', format = TRUE)
	})

	#####################
	tkgrid(cbprd.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep1.tab1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)

	tkgrid(lab1.tab1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dirCDF.tab1, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bdirCDF.tab1, row = 3, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(cap1.tab1, row = 4, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(netCDFff.tab1, row = 4, column = 2, sticky = 'w', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep2.tab1, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)

	tkgrid(lab2.tab1, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cbSHP.tab1, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(btSHP.tab1, row = 7, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(lab3.tab1, row = 8, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(adminVar.tab1, row = 9, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep3.tab1, row = 10, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)
	tkgrid(open.tab1, row = 11, column = 0, sticky = 'we', rowspan = 1, columnspan = 3)
	tkgrid(display.tab1, row = 11, column = 3, sticky = 'we', rowspan = 1, columnspan = 3)

	#####################
	infobulle(netCDFff.tab1, 'Enter the format of the NetCDF files names,\nexample: rr_mrg_1983011.nc')
	status.bar.display(netCDFff.tab1, TextOutputVar, 'Enter the format of the NetCDF files names, example: rr_mrg_1983011.nc')

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

	##############
	dayLabTab2_Var <- tclVar('Dek')

	istart_yrs <- tclVar('1983')
	istart_mon <- tclVar('1')
	istart_day <- tclVar('1')
	iend_yrs <- tclVar('2014')
	iend_mon <- tclVar('12')
	iend_day <- tclVar('3')
	output_TSformat <- tclVar('Dekadal')

	# MonthsName <- format(ISOdate(2014,1:12,1), "%B")
	MonthsName <- format(ISOdate(2014, 1:12, 1), "%b")
	use_mois1 <- tclVar(MonthsName[1])
	use_mois2 <- tclVar(MonthsName[12])

	start_mois <- tclVar(MonthsName[12])
	end_mois <- tclVar(MonthsName[2])
	aggrFun <- tclVar('mean')
	MissFrac <- tclVar('0.95')

	calc.anomaly <- tclVar(0)
	calc.stanomaly <- tclVar(0)
	calc.climato <- tclVar(0)

	##############
	lab1.tab2 <- tklabel(subfr2, text = "Date Range", bg = 'green')
	debLab.tab2 <- tklabel(subfr2, text = 'Start date', anchor = 'e', justify = 'right')
	finLab.tab2 <- tklabel(subfr2, text = 'End date', anchor = 'e', justify = 'right')
	yrsLab.tab2 <- tklabel(subfr2, text = 'Year')
	monLab.tab2 <- tklabel(subfr2, text = 'Month')
	dayLab.tab2 <- tklabel(subfr2, text = tclvalue(dayLabTab2_Var), textvariable = dayLabTab2_Var)

	yrs1.tab2 <- tkentry(subfr2, width = 4, textvariable = istart_yrs, justify = "left")
	mon1.tab2 <- tkentry(subfr2, width = 4, textvariable = istart_mon, justify = "left")
	day1.tab2 <- tkentry(subfr2, width = 4, textvariable = istart_day, justify = "left")
	yrs2.tab2 <- tkentry(subfr2, width = 4, textvariable = iend_yrs, justify = "left")
	mon2.tab2 <- tkentry(subfr2, width = 4, textvariable = iend_mon, justify = "left")
	day2.tab2 <- tkentry(subfr2, width = 4, textvariable = iend_day, justify = "left")

	usemonth.tab2 <- tkframe(subfr2)
	usemonLab.tab2 <- tklabel(usemonth.tab2, text = 'Use months :', bg = 'green')
	usemon1Lab.tab2 <- tklabel(usemonth.tab2, text = 'from')
	usemon2Lab.tab2 <- tklabel(usemonth.tab2, text = 'to')
	usemon1Cb.tab2 <- ttkcombobox(usemonth.tab2, values = MonthsName, textvariable = use_mois1, width = 5)
	usemon2Cb.tab2 <- ttkcombobox(usemonth.tab2, values = MonthsName, textvariable = use_mois2, width = 5)

	sep1.tab2 <- ttkseparator(subfr2)
	lab2.tab2 <- tklabel(subfr2, text = "Output time series frequency", anchor = 'w', justify = 'left')
	cbOutTS.tab2 <- ttkcombobox(subfr2, values = c('Dekadal', 'Monthly', '3-Months', '6-Months', 'Yearly'), textvariable = output_TSformat, width = largeur)

	sep2.tab2 <- ttkseparator(subfr2)
	seasLab.tab2 <- tklabel(subfr2, text = 'Season months', anchor = 'w', justify = 'left')
	cbChoixM1.tab2 <- ttkcombobox(subfr2, values = MonthsName, textvariable = start_mois, state = 'disabled', width = 4)
	cbChoixM2.tab2 <- ttkcombobox(subfr2, values = MonthsName, textvariable = end_mois, state = 'disabled', width = 4)

	sep3.tab2 <- ttkseparator(subfr2)
	CbAggreFun.tab2 <- ttkcombobox(subfr2, values = c("mean", "sum"), textvariable = aggrFun, width = 10)
	MissFracLab.tab2 <- tklabel(subfr2, text = 'Min.frac', anchor = 'e', justify = 'right')
	MissFracVal.tab2 <- tkentry(subfr2, width = 4, textvariable = MissFrac, justify = "left")

	sep4.tab2 <- ttkseparator(subfr2)
	calcAnomaly.tab2 <- tkcheckbutton(subfr2, variable = calc.anomaly, text = "Compute Anomalies", anchor = 'w', justify = 'left')
	calcStAnomaly.tab2 <- tkcheckbutton(subfr2, variable = calc.stanomaly, text = "Compute Standardized Anomalies", anchor = 'w', justify = 'left')
	calcClimato.tab2 <- tkcheckbutton(subfr2, variable = calc.climato, text = "Compute Climatologies", anchor = 'w', justify = 'left')

	##############
	tkgrid(lab1.tab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrsLab.tab2, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(monLab.tab2, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dayLab.tab2, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(debLab.tab2, row = 1, column = 1, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs1.tab2, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon1.tab2, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day1.tab2, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(finLab.tab2, row = 2, column = 1, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs2.tab2, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon2.tab2, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day2.tab2, row = 2, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(usemonth.tab2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 3)
	tkgrid(usemonLab.tab2, usemon1Lab.tab2, usemon1Cb.tab2, usemon2Lab.tab2, usemon2Cb.tab2)

	tkgrid(sep1.tab2, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)
	tkgrid(lab2.tab2, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cbOutTS.tab2, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep2.tab2, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)
	tkgrid(seasLab.tab2, row = 8, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cbChoixM1.tab2, row = 8, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cbChoixM2.tab2, row = 8, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep3.tab2, row = 9, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)
	tkgrid(CbAggreFun.tab2, row = 10, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(MissFracLab.tab2, row = 10, column = 3, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(MissFracVal.tab2, row = 10, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep4.tab2, row = 11, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)
	tkgrid(calcAnomaly.tab2, row = 12, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(calcStAnomaly.tab2, row = 13, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(calcClimato.tab2, row = 14, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##############

	infobulle(CbAggreFun.tab2, 'Function to be used to compute dekadal, monthly and yearly series')
	status.bar.display(CbAggreFun.tab2, TextOutputVar, 'Function to be used to convert dekadal, monthly and yearly series')
	infobulle(MissFracLab.tab2, 'Minimum fraction of available data that must be present for the time period to compute')
	status.bar.display(MissFracLab.tab2, TextOutputVar, 'Minimum fraction of available data that must be present for the time period to compute')
	infobulle(MissFracVal.tab2, 'Minimum fraction of available data that must be present for the time period to compute')
	status.bar.display(MissFracVal.tab2, TextOutputVar, 'Minimum fraction of available data that must be present for the time period to compute')

	infobulle(calcAnomaly.tab2, 'Compute Anomalies from the climatology')
	status.bar.display(calcAnomaly.tab2, TextOutputVar, 'Compute Anomalies from the climatology')
	infobulle(calcStAnomaly.tab2, 'Compute Standardize Anomalies from the climatology')
	status.bar.display(calcStAnomaly.tab2, TextOutputVar, 'Compute Standardize Anomalies from the climatology')
	infobulle(calcClimato.tab2, 'Calculate Climatologies')
	status.bar.display(calcClimato.tab2, TextOutputVar, 'Calculate Climatologies')

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

	##########################
	area_type <<- tclVar('Point')

	pointrect <- tclVar('Point')
	minrect <- tclVar()
	maxrect <- tclVar()

	minlonRect <<- tclVar()
	maxlonRect <<- tclVar()
	minlatRect <<- tclVar()
	maxlatRect <<- tclVar()

	pmLon <- tclVar('0.0')
	pmLat <- tclVar('0.0')

	namePoly <<- tclVar()

	##########################

	xentr1.tab3 <- tkentry(subfr3, width = 7, justify = "left", textvariable = xx1)
	xentr2.tab3 <- tkentry(subfr3, width = 7, justify = "left", textvariable = xx2)
	yentr1.tab3 <- tkentry(subfr3, width = 7, justify = "left", textvariable = yy1)
	yentr2.tab3 <- tkentry(subfr3, width = 7, justify = "left", textvariable = yy2)
	btCentre.tab3 <- tklabel(subfr3, image = pikCentre)

	btZoomP.tab3 <<- tkbutton(subfr3, image = pikZoomPlus, relief = 'raised', bg = 'lightblue', state = 'normal')
	btZoomM.tab3 <<- tkbutton(subfr3, image = pikZoomMinus, relief = 'raised', bg = 'lightblue', state = 'normal')
	btZoomRect.tab3 <<- tkbutton(subfr3, image = pikZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
	btPanImg.tab3 <<- tkbutton(subfr3, image = pikPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
	btRedraw.tab3 <- tkbutton(subfr3, image = pikRedraw, relief = 'raised', bg = 'lightblue')
	btReset.tab3 <- tkbutton(subfr3, image = pikReset, relief = 'raised')

	sep1.tab3 <- ttkseparator(subfr3)
	lab1.tab3 <- tklabel(subfr3, text = "Extract Type", anchor = 'w', justify = 'left')
	cbAreaType.tab3 <- ttkcombobox(subfr3, values = c('Point', 'Multiple Points', 'Rectangle', 'Polygon', 'Multiple Polygons'), textvariable = area_type, width = 10)
	getArea.tab3 <<- tkbutton(subfr3, text = "GET", relief = 'raised', bg = 'lightblue', state = 'normal')

	sep2.tab3 <- ttkseparator(subfr3)
	lab2.tab3 <- tklabel(subfr3, text = tclvalue(pointrect), textvariable = pointrect, anchor = 'w', justify = 'left', bg = 'green')
	minLab.tab3 <- tklabel(subfr3, text = tclvalue(minrect), textvariable = minrect)
	maxLab.tab3 <- tklabel(subfr3, text = tclvalue(maxrect), textvariable = maxrect)
	lonLab.tab3 <- tklabel(subfr3, text = 'Lon', anchor = 'e', justify = 'right')
	latLab.tab3 <- tklabel(subfr3, text = 'Lat', anchor = 'e', justify = 'right')

	addObjs.tab3 <- tkbutton(subfr3, text = "ADD", relief = 'raised', bg = 'lightblue', state = 'normal')

	minlon.tab3 <- tkentry(subfr3, width = 7, textvariable = minlonRect, justify = "left")
	maxlon.tab3 <- tkentry(subfr3, width = 7, textvariable = maxlonRect, justify = "left", state = 'disabled')
	minlat.tab3 <- tkentry(subfr3, width = 7, textvariable = minlatRect, justify = "left")
	maxlat.tab3 <- tkentry(subfr3, width = 7, textvariable = maxlatRect, justify = "left", state = 'disabled')

	pmLab1.tab3 <- tklabel(subfr3, text = '\u00B1', anchor = 'e', justify = 'right')
	pmLab2.tab3 <- tklabel(subfr3, text = '\u00B1', anchor = 'e', justify = 'right')

	pmLon.tab3 <- tkentry(subfr3, width = 4, textvariable = pmLon, justify = "left")
	pmLat.tab3 <- tkentry(subfr3, width = 4, textvariable = pmLat, justify = "left")

	sep3.tab3 <- ttkseparator(subfr3)
	lab3.tab3 <- tklabel(subfr3, text = "Polygon", anchor = 'w', justify = 'left', bg = 'green')
	cbpolyType.tab3 <- ttkcombobox(subfr3, values = '', textvariable = namePoly, width = 10, state = 'disabled')

	##########################

	tkgrid(xentr1.tab3, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(xentr2.tab3, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(yentr1.tab3, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(yentr2.tab3, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(btCentre.tab3, row = 1, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1)

	tkgrid(btReset.tab3, row = 0, column = 3, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(btRedraw.tab3, row = 1, column = 3, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(btPanImg.tab3, row = 2, column = 3, sticky = 'nswe', rowspan = 1, columnspan = 1)

	tkgrid(btZoomP.tab3, row = 0, column = 4, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(btZoomM.tab3, row = 1, column = 4, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(btZoomRect.tab3, row = 2, column = 4, sticky = 'nswe', rowspan = 1, columnspan = 1)

	tkgrid(sep1.tab3, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, pady = 5)
	tkgrid(lab1.tab3, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5)

	tkgrid(cbAreaType.tab3, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4)
	tkgrid(getArea.tab3, row = 5, column = 4, rowspan = 1, columnspan = 1) #,sticky = 'we'

	tkgrid(sep2.tab3, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, pady = 5)

	tkgrid(lab2.tab3, row = 7, column = 0, sticky = 'w', rowspan = 1, columnspan = 1)
	tkgrid(minLab.tab3, row = 7, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(maxLab.tab3, row = 7, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)

	tkgrid(addObjs.tab3, row = 7, column = 4, rowspan = 1, columnspan = 1) #,sticky = 'we'

	tkgrid(lonLab.tab3, row = 8, column = 0, sticky = 'e', rowspan = 1, columnspan = 1)
	tkgrid(minlon.tab3, row = 8, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(maxlon.tab3, row = 8, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)

	tkgrid(pmLab1.tab3, row = 8, column = 3, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(pmLon.tab3, row = 8, column = 4, sticky = 'we', rowspan = 1, columnspan = 1)

	tkgrid(latLab.tab3, row = 9, column = 0, sticky = 'e', rowspan = 1, columnspan = 1)
	tkgrid(minlat.tab3, row = 9, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(maxlat.tab3, row = 9, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)

	tkgrid(pmLab2.tab3, row = 9, column = 3, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(pmLat.tab3, row = 9, column = 4, sticky = 'we', rowspan = 1, columnspan = 1)

	tkgrid(sep3.tab3, row = 10, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, pady = 5)
	tkgrid(lab3.tab3, row = 11, column = 0, sticky = 'w', rowspan = 1, columnspan = 5)
	tkgrid(cbpolyType.tab3, row = 11, column = 1, sticky = 'we', rowspan = 1, columnspan = 4)

	##########################

	infobulle(btZoomP.tab3, 'Zoom In')
	status.bar.display(btZoomP.tab3, TextOutputVar, 'Zoom In')
	infobulle(btZoomM.tab3, 'Zoom Out')
	status.bar.display(btZoomM.tab3, TextOutputVar, 'Zoom Out')
	infobulle(btZoomRect.tab3, 'Zoom Area')
	status.bar.display(btZoomRect.tab3, TextOutputVar, 'Zoom Area')
	infobulle(btPanImg.tab3, 'Pan Tool')
	status.bar.display(btPanImg.tab3, TextOutputVar, 'Pan Tool')
	infobulle(btRedraw.tab3, 'Redraw Map')
	status.bar.display(btRedraw.tab3, TextOutputVar, 'Redraw Map')
	infobulle(btReset.tab3,' Zoom Reset')
	status.bar.display(btReset.tab3, TextOutputVar,' Zoom Reset')

	infobulle(getArea.tab3, 'Before to click on the map to select object,\nclick here to activate the function')
	status.bar.display(getArea.tab3, TextOutputVar, 'Before to click on the map to select object, click here to activate the function')

	infobulle(addObjs.tab3, 'Multiple Points&Polygons: after selecting object from the map\nclick here to add the object to the list')
	status.bar.display(addObjs.tab3, TextOutputVar, 'Multiple Points&Polygons: after selecting object from the map click here to add the object to the list')

	infobulle(pmLon.tab3, 'Add value in decimal degree to get\nrectangle centered at the target points')
	status.bar.display(pmLon.tab3, TextOutputVar, 'Add value in decimal degree to get rectangle centered at the target points')
	infobulle(pmLat.tab3, 'Add value in decimal degree to get rectangle\ncentered at the target points')
	status.bar.display(pmLat.tab3, TextOutputVar, 'Add value in decimal degree to get rectangle centered at the target points')

	##########################
	tkconfigure(btRedraw.tab3, command = function(){
		ZoomXYval <<- as.numeric(c(tclvalue(xx1), tclvalue(xx2), tclvalue(yy1), tclvalue(yy2)))
		tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))+1
		if(length(AllOpenTabType) > 0){
			if(AllOpenTabType[[tabid]] == "img" & !is.null(notebookTab)){
				if(AllOpenTabData[[tabid]][[1]][[1]]$ID  == notebookTab[[2]]){
					assign("ZoomXYval", ZoomXYval, envir = environment(AllOpenTabData[[tabid]][[2]][[2]]$fun))
					refreshPlot1(W = AllOpenTabData[[tabid]][[2]][[1]],
								img = AllOpenTabData[[tabid]][[2]][[2]],
								hscale = as.numeric(tclvalue(tkget(spinH))),
								vscale = as.numeric(tclvalue(tkget(spinV))))
					tkconfigure(btRedraw.tab3, relief = 'raised', bg = 'lightblue')
				}
			}
		}
	})

	##########################
	tkconfigure(btReset.tab3, command = function(){
		ZoomXYval <<- ZoomXYval0
		tclvalue(xx1) <<- ZoomXYval0[1]
		tclvalue(xx2) <<- ZoomXYval0[2]
		tclvalue(yy1) <<- ZoomXYval0[3]
		tclvalue(yy2) <<- ZoomXYval0[4]
		tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))+1
		if(length(AllOpenTabType) > 0){
			if(AllOpenTabType[[tabid]] == "img" & !is.null(notebookTab)){
				if(AllOpenTabData[[tabid]][[1]][[1]]$ID  == notebookTab[[2]]){
					assign("ZoomXYval", ZoomXYval, envir = environment(AllOpenTabData[[tabid]][[2]][[2]]$fun))
					refreshPlot1(W = AllOpenTabData[[tabid]][[2]][[1]],
								img = AllOpenTabData[[tabid]][[2]][[2]],
								hscale = as.numeric(tclvalue(tkget(spinH))),
								vscale = as.numeric(tclvalue(tkget(spinV))))
					tkconfigure(btRedraw.tab3, relief = 'raised', bg = 'lightblue')
				}
			}
		}
	})

	##########################
	pressButP <<- tclVar('0')
	pressButM <<- tclVar('0')
	pressButRect <<- tclVar('0')
	pressButDrag <<- tclVar('0')
	pressGetCoords <<- tclVar('0')

	tkbind(btRedraw.tab3, "<Button-1>", function(){
		tclvalue(pressButP) <<- 0
		tclvalue(pressButM) <<- 0
		tclvalue(pressButRect) <<- 0
		tclvalue(pressButDrag) <<- 0
		tclvalue(pressGetCoords) <<- 0
		tkconfigure(btRedraw.tab3, relief = 'raised', bg = 'lightblue')
		tkconfigure(btZoomP.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomM.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomRect.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btPanImg.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(getArea.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
	})

	tkbind(btReset.tab3, "<Button-1>", function(){
		tclvalue(pressButP) <<- 0
		tclvalue(pressButM) <<- 0
		tclvalue(pressButRect) <<- 0
		tclvalue(pressButDrag) <<- 0
		tclvalue(pressGetCoords) <<- 0
		tkconfigure(btRedraw.tab3, relief = 'raised', bg = 'lightblue')
		tkconfigure(btZoomP.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomM.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomRect.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btPanImg.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(getArea.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
	})

	tkbind(btZoomP.tab3, "<Button-1>", function(){
		tclvalue(pressButP) <<- 1
		tclvalue(pressButM) <<- 0
		tclvalue(pressButRect) <<- 0
		tclvalue(pressButDrag) <<- 0
		tclvalue(pressGetCoords) <<- 0
		tkconfigure(btRedraw.tab3, relief = 'raised', bg = 'lightblue')
		tkconfigure(btZoomP.tab3, relief = 'raised', bg = 'red', state = 'disabled')
		tkconfigure(btZoomM.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomRect.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btPanImg.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(getArea.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
	})

	tkbind(btZoomM.tab3, "<Button-1>", function(){
		tclvalue(pressButP) <<- 0
		tclvalue(pressButM) <<- 1
		tclvalue(pressButRect) <<- 0
		tclvalue(pressButDrag) <<- 0
		tclvalue(pressGetCoords) <<- 0
		tkconfigure(btRedraw.tab3, relief = 'raised', bg = 'lightblue')
		tkconfigure(btZoomP.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomM.tab3, relief = 'raised', bg = 'red', state = 'disabled')
		tkconfigure(btZoomRect.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btPanImg.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(getArea.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
	})

	tkbind(btZoomRect.tab3, "<Button-1>", function(){
		tclvalue(pressButP) <<- 0
		tclvalue(pressButM) <<- 0
		tclvalue(pressButRect) <<- 1
		tclvalue(pressButDrag) <<- 0
		tclvalue(pressGetCoords) <<- 0
		tkconfigure(btRedraw.tab3, relief = 'raised', bg = 'lightblue')
		tkconfigure(btZoomP.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomM.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomRect.tab3, relief = 'raised', bg = 'red', state = 'disabled')
		tkconfigure(btPanImg.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(getArea.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
	})

	tkbind(btPanImg.tab3, "<Button-1>", function(){
		tclvalue(pressButP) <<- 0
		tclvalue(pressButM) <<- 0
		tclvalue(pressButRect) <<- 0
		tclvalue(pressButDrag) <<- 1
		tclvalue(pressGetCoords) <<- 0
		tkconfigure(btRedraw.tab3, relief = 'raised', bg = 'lightblue')
		tkconfigure(btZoomP.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomM.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomRect.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btPanImg.tab3, relief = 'raised', bg = 'red', state = 'disabled')
		tkconfigure(getArea.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
	})

	tkbind(getArea.tab3, "<Button-1>", function(){
		tclvalue(pressButP) <<- 0
		tclvalue(pressButM) <<- 0
		tclvalue(pressButRect) <<- 0
		tclvalue(pressButDrag) <<- 0
		tclvalue(pressGetCoords) <<- 1
		tkconfigure(btRedraw.tab3, relief = 'raised', bg = 'lightblue')
		tkconfigure(btZoomP.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomM.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomRect.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btPanImg.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(getArea.tab3, relief = 'raised', bg = 'red', state = 'disabled')
	})

	#####
	initializeButZoom <- function(){
			initXYval0 <<- str_trim(c(tclvalue(xx1), tclvalue(xx2), tclvalue(yy1), tclvalue(yy2)))
			tclvalue(pressButP) <<- 0
			tclvalue(pressButM) <<- 0
			tclvalue(pressButRect) <<- 0
			tclvalue(pressButDrag) <<- 0
			tclvalue(pressGetCoords) <<- 0
			tkconfigure(btZoomP.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(btZoomM.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(btZoomRect.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(btPanImg.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(getArea.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
	}

	activateButRedraw <- function(){
		initXYval1 <- str_trim(c(tclvalue(xx1), tclvalue(xx2), tclvalue(yy1), tclvalue(yy2)))
		if(!all(initXYval0 == initXYval1)) tkconfigure(btRedraw.tab3, relief = 'raised', bg = 'red')
	}

	####
	tkbind(xentr1.tab3, "<FocusIn>", initializeButZoom)
	tkbind(xentr1.tab3, "<FocusOut>", activateButRedraw)

	tkbind(xentr2.tab3, "<FocusIn>", initializeButZoom)
	tkbind(xentr2.tab3, "<FocusOut>", activateButRedraw)

	tkbind(yentr1.tab3, "<FocusIn>", initializeButZoom)
	tkbind(yentr1.tab3, "<FocusOut>", activateButRedraw)

	tkbind(yentr2.tab3, "<FocusIn>", initializeButZoom)
	tkbind(yentr2.tab3, "<FocusOut>", activateButRedraw)

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

	##########################

	spatAverage <- tclVar('1')
	ChoixOutType <- tclVar('1')
	fileORdir <- tclVar('File to save extracted data')
	file.save1 <- tclVar()

	##########################
	lab1.tab4 <- tklabel(subfr4, text = 'Saptially Average Over Selected Area', anchor = 'e', justify = 'right')
	chkSpaAvrg.tab4 <- tkcheckbutton(subfr4, variable = spatAverage)

	sep1.tab4 <- ttkseparator(subfr4)
	outputype.tab4 <- ttklabelframe(subfr4, text = "Output File Formats", labelanchor = "nw", relief = "groove", borderwidth = 2)

	sep2.tab4 <- ttkseparator(subfr4)
	lab2.tab4 <- tklabel(subfr4, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
	fl2sav.tab4 <- tkentry(subfr4, textvariable = file.save1, width = largeur)
	bfl2sav.tab4 <- tkbutton(subfr4, text = "...")

	sep3.tab4 <- ttkseparator(subfr4)
	excute.tab4 <- tkbutton(subfr4, text = "EXTRACT TS")

	##########################
	outTypeRadio1 <- tkradiobutton(outputype.tab4, text = "Separate Files Matrix", anchor = 'w', justify = 'left')
	outTypeRadio2 <- tkradiobutton(outputype.tab4, text = "One File Matrix", anchor = 'w', justify = 'left')
	outTypeRadio3 <- tkradiobutton(outputype.tab4, text = "One File Time|Lat|Lon|Value", anchor = 'w', justify = 'left')

	tkconfigure(outTypeRadio1, variable = ChoixOutType, value = "1")
	tkconfigure(outTypeRadio2, variable = ChoixOutType, value = "2")
	tkconfigure(outTypeRadio3, variable = ChoixOutType, value = "3")

	tkgrid(outTypeRadio1, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(outTypeRadio2, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(outTypeRadio3, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)

	##########################

	tkconfigure(bfl2sav.tab4, command = function(){
		fileORdir2Save(file.save1, isFile = TRUE)
	})

	tkconfigure(excute.tab4, command = function(){
		# #si utiliser avec Run toolbar button ( <<- ) and initialize
 		retExtractParams <- list(freq = tclvalue(file.period),
					out.ts = tclvalue(output_TSformat),
					ncdat = list(dir = tclvalue(dirNetCDF), format = tclvalue(netCDFff)),
					shpdat = list(shpf = tclvalue(Admin_shp), attr = tclvalue(Admin_var), id = tclvalue(namePoly)),
					dates = c(tclvalue(istart_yrs), tclvalue(istart_mon), tclvalue(istart_day),
								tclvalue(iend_yrs), tclvalue(iend_mon), tclvalue(iend_day)),
					usemon = list(start = tclvalue(use_mois1), end = tclvalue(use_mois2)),
					seasmon = list(start = tclvalue(start_mois), end = tclvalue(end_mois)),
					aggre = list(fun = tclvalue(aggrFun), missfrac = tclvalue(MissFrac)),
					rect = c(tclvalue(minlonRect), tclvalue(maxlonRect), tclvalue(minlatRect),tclvalue(maxlatRect)),
					climato = list(anom = tclvalue(calc.anomaly), stanom = tclvalue(calc.stanomaly), clim = tclvalue(calc.climato)),
					pts.int = c(tclvalue(pmLon), tclvalue(pmLat)),
					polyg = if(is.null(EnvMultiPP$multiptspoly)) NA else EnvMultiPP$multiptspoly,
					area.type = tclvalue(area_type),
					sp.ave = tclvalue(spatAverage),
					out.type = tclvalue(ChoixOutType),
					outdir = tclvalue(file.save1))

		# assign('retExtractParams', retExtractParams, envir = .GlobalEnv)

		tkconfigure(main.win, cursor = 'watch')
		InsertMessagesTxt(main.txt.out, "Extraction.................")
		tcl('update')
		ret <- tryCatch(getExtractDataFun(retExtractParams),
		#warning = function(w) warningFun(w),
		error = function(e) errorFun(e),
		finally = {
			tkconfigure(main.win, cursor = '')
		})

		if(!is.null(ret)){
			if(ret == 0) InsertMessagesTxt(main.txt.out, "Extraction finished successfully")
			else InsertMessagesTxt(main.txt.out, "Extraction failed", format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, "Extraction failed", format = TRUE)
		}
	})

	##########################
	tkgrid(lab1.tab4, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chkSpaAvrg.tab4, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep1.tab4, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, pady = 5)
	tkgrid(lab2.tab4, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fl2sav.tab4, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bfl2sav.tab4, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep2.tab4, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, pady = 5)
	tkgrid(outputype.tab4, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep3.tab4, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, pady = 5)
	tkgrid(excute.tab4, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##########################

	infobulle(outputype.tab4, 'Select the file format to save result in case of Rectangle or Polygon')
	status.bar.display(outputype.tab4, TextOutputVar, 'Select the file format to save result in case of Rectangle or Polygon')

	infobulle(fl2sav.tab4, 'Enter the full path to the directory or file to save result')
	status.bar.display(fl2sav.tab4, TextOutputVar, 'Enter the full path to the directory or file to save extracted data')
	infobulle(bfl2sav.tab4, 'Browse here the full path to the directory or file to save result')
	status.bar.display(bfl2sav.tab4, TextOutputVar, 'Browse here the full path to the directory or file to save extracted data')

	######################################################################################################

	tkbind(cbprd.tab1, "<<ComboboxSelected>>", function(){
		if(tclvalue(file.period) == 'Daily data'){
			tclvalue(netCDFff) <- "rr_mrg_%s%s%s.nc"
			infobulle(netCDFff.tab1, 'Enter the format of the NetCDF files names,\nexample: rr_mrg_19830101.nc')
			status.bar.display(netCDFff.tab1, TextOutputVar, 'Enter the format of the NetCDF files names, example: rr_mrg_19830101.nc')
			tclvalue(dayLabTab2_Var) <- 'Day'
			tkconfigure(day1.tab2, state = 'normal')
			tkconfigure(day2.tab2, state = 'normal')
			tkconfigure(cbOutTS.tab2, values = c('Daily', 'Dekadal', 'Monthly', '3-Months', '6-Months', 'Yearly'))
			tclvalue(output_TSformat) <- 'Daily'
			tclvalue(iend_day) <- '31'
		}
		if(tclvalue(file.period) == 'Dekadal data'){
			tclvalue(netCDFff) <- "rr_mrg_%s%s%s.nc"
			infobulle(netCDFff.tab1, 'Enter the format of the NetCDF files names,\nexample: rr_mrg_1983011.nc')
			status.bar.display(netCDFff.tab1, TextOutputVar, 'Enter the format of the NetCDF files names, example: rr_mrg_1983011.nc')
			tclvalue(dayLabTab2_Var) <- 'Dek'
			tkconfigure(day1.tab2, state = 'normal')
			tkconfigure(day2.tab2, state = 'normal')
			tkconfigure(cbOutTS.tab2, values = c('Dekadal', 'Monthly', '3-Months', '6-Months', 'Yearly'))
			tclvalue(output_TSformat) <- 'Dekadal'
			tclvalue(iend_day) <- '3'
		}
		if(tclvalue(file.period) == 'Monthly data'){
			tclvalue(netCDFff) <- "rr_mrg_%s%s.nc"
			infobulle(netCDFff.tab1, 'Enter the format of the NetCDF files names,\nexample: rr_mrg_198301.nc')
			status.bar.display(netCDFff.tab1, TextOutputVar, 'Enter the format of the NetCDF files names, example: rr_mrg_198301.nc')
			tkconfigure(day1.tab2, state = 'disabled')
			tkconfigure(day2.tab2, state = 'disabled')
			tkconfigure(cbOutTS.tab2, values = c('Monthly', '3-Months', '6-Months', 'Yearly'))
			tclvalue(output_TSformat) <- 'Monthly'
		}
	})


	#######################
	tkbind(cbSHP.tab1, "<<ComboboxSelected>>", function(){
		shpf <- getShpOpenData(Admin_shp)
		dat <- shpf[[2]]@data
		shpAttr <- names(dat)
		tclvalue(Admin_var) <- shpAttr[1]
		tkconfigure(adminVar.tab1, values = shpAttr, textvariable = Admin_var)
	})

	#############################
	tkbind(adminVar.tab1, "<<ComboboxSelected>>", function(){
		shpf <- getShpOpenData(Admin_shp)
		dat <- shpf[[2]]@data
		adminN <- as.character(dat[, as.numeric(tclvalue(tcl(adminVar.tab1, 'current')))+1])
		shpAttr <- levels(as.factor(adminN))
		tclvalue(namePoly) <- shpAttr[1]
		tkconfigure(cbpolyType.tab3, values = shpAttr, textvariable = namePoly)
	})

	#######################
	tkbind(cbOutTS.tab2, "<<ComboboxSelected>>", function(){

		if(tclvalue(output_TSformat) == '3-Months'){
			tkconfigure(cbChoixM1.tab2, state = 'normal')
			tclvalue(end_mois) <- cycleMonth(tclvalue(start_mois), 3)
		}else if(tclvalue(output_TSformat) == '6-Months'){
			tkconfigure(cbChoixM1.tab2, state = 'normal')
			tclvalue(end_mois) <- cycleMonth(tclvalue(start_mois), 6)
		}else{
			tkconfigure(cbChoixM1.tab2, state = 'disabled')
		}

	})

	############################
	tkbind(cbChoixM1.tab2, "<<ComboboxSelected>>", function(){
		if(tclvalue(output_TSformat) == '3-Months') tclvalue(end_mois) <- cycleMonth(tclvalue(start_mois), 3)
		if(tclvalue(output_TSformat) == '6-Months') tclvalue(end_mois) <- cycleMonth(tclvalue(start_mois), 6)
	})

	############################

	retMultiP <- NULL

	tkbind(cbAreaType.tab3, "<<ComboboxSelected>>", function(){
		if(tclvalue(area_type) == 'Point'){
			if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)

			tkconfigure(minlon.tab3, state = 'normal')
			tkconfigure(maxlon.tab3, state = 'disabled')
			tkconfigure(minlat.tab3, state = 'normal')
			tkconfigure(maxlat.tab3, state = 'disabled')
			tkconfigure(pmLon.tab3, state = 'normal')
			tkconfigure(pmLat.tab3, state = 'normal')
			tkconfigure(cbpolyType.tab3, state = 'disabled')
			tclvalue(pointrect) <- 'Point'
			tclvalue(minrect) <- ''
			tclvalue(maxrect) <- ''
			selectedPolygon <- NULL

			tclvalue(fileORdir) <- 'File to save extracted data'
			tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = TRUE))
		}

		##
		if(tclvalue(area_type) == 'Rectangle'){
			if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)

			tkconfigure(minlon.tab3, state = 'normal')
			tkconfigure(maxlon.tab3, state = 'normal')
			tkconfigure(minlat.tab3, state = 'normal')
			tkconfigure(maxlat.tab3, state = 'normal')
			tkconfigure(pmLon.tab3, state = 'disabled')
			tkconfigure(pmLat.tab3, state = 'disabled')
			tkconfigure(cbpolyType.tab3, state = 'disabled')
			tclvalue(pointrect) <- 'Rectangle'
			tclvalue(minrect) <- 'Min'
			tclvalue(maxrect) <- 'Max'
			selectedPolygon <- NULL

			if(tclvalue(spatAverage) == '1'){
				tclvalue(fileORdir) <- 'File to save extracted data'
				tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = TRUE))
			}else{
				tclvalue(fileORdir) <- 'Directory to save extracted data'
				tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = FALSE))
			}
		}

		##
		if(tclvalue(area_type) == 'Polygon'){
			if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)

			tkconfigure(minlon.tab3, state = 'disabled')
			tkconfigure(maxlon.tab3, state = 'disabled')
			tkconfigure(minlat.tab3, state = 'disabled')
			tkconfigure(maxlat.tab3, state = 'disabled')
			tkconfigure(pmLon.tab3, state = 'disabled')
			tkconfigure(pmLat.tab3, state = 'disabled')
			tkconfigure(cbpolyType.tab3, state = 'normal')
			tclvalue(minrect) <- ''
			tclvalue(maxrect) <- ''

			if(tclvalue(spatAverage) == '1'){
				tclvalue(fileORdir) <- 'File to save extracted data'
				tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = TRUE))
			}else{
				tclvalue(fileORdir) <- 'Directory to save extracted data'
				tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = FALSE))
			}

			if(tclvalue(namePoly) != ''){
				shpfopen <- getShpOpenData(Admin_shp)
				shpf <- shpfopen[[2]]
				ids <- as.numeric(tclvalue(tcl(adminVar.tab1, 'current')))+1
				selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(namePoly), ])
			}else{
				selectedPolygon <- NULL
			}
		}

		##
		if(tclvalue(area_type) == 'Multiple Points'){
			if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)
			retMultiP <<- previewWin(main.win, c('normal', 'disabled'), list(adminVar.tab1, Admin_shp))

			tkconfigure(minlon.tab3, state = 'normal')
			tkconfigure(maxlon.tab3, state = 'disabled')
			tkconfigure(minlat.tab3, state = 'normal')
			tkconfigure(maxlat.tab3, state = 'disabled')
			tkconfigure(pmLon.tab3, state = 'normal')
			tkconfigure(pmLat.tab3, state = 'normal')
			tkconfigure(cbpolyType.tab3, state = 'disabled')
			tclvalue(pointrect) <- 'Point'
			tclvalue(minrect) <- ''
			tclvalue(maxrect) <- ''
			selectedPolygon <- NULL

			tclvalue(fileORdir) <- 'File to save extracted data'
			tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = TRUE))
		}

		##
		if(tclvalue(area_type) == 'Multiple Polygons'){
			if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)
			retMultiP <<- previewWin(main.win, c('disabled', 'normal'), list(adminVar.tab1, Admin_shp))

			tkconfigure(minlon.tab3, state = 'disabled')
			tkconfigure(maxlon.tab3, state = 'disabled')
			tkconfigure(minlat.tab3, state = 'disabled')
			tkconfigure(maxlat.tab3, state = 'disabled')
			tkconfigure(pmLon.tab3, state = 'disabled')
			tkconfigure(pmLat.tab3, state = 'disabled')
			tkconfigure(cbpolyType.tab3, state = 'normal')
			tclvalue(minrect) <- ''
			tclvalue(maxrect) <- ''

			tclvalue(fileORdir) <- 'File to save extracted data'
			tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = TRUE))

			if(tclvalue(namePoly) != ''){
				shpfopen <- getShpOpenData(Admin_shp)
				shpf <- shpfopen[[2]]
				ids <- as.numeric(tclvalue(tcl(adminVar.tab1, 'current')))+1
				selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(namePoly), ])
			}else{
				selectedPolygon <- NULL
			}
		}

		##
		tclvalue(minlonRect) <<- ''
		tclvalue(maxlonRect) <<- ''
		tclvalue(minlatRect) <<- ''
		tclvalue(maxlatRect) <<- ''
		tkconfigure(getArea.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')

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
	nbpts <- 1
	tkconfigure(addObjs.tab3, command = function(){
		if(tclvalue(area_type) == 'Multiple Points'){			
			tkinsert(retMultiP$textObj, "end", paste(paste('Pts', nbpts, sep = ''), tclvalue(minlonRect), tclvalue(minlatRect), "\n"))
			nbpts <<- nbpts+1
		}
		if(tclvalue(area_type) == 'Multiple Polygons') tkinsert(retMultiP$textObj, "end", paste(tclvalue(namePoly), "\n"))
	})

	#############################
	tkbind(cbpolyType.tab3, "<<ComboboxSelected>>", function(){
		if(tclvalue(namePoly) != ''){
			shpfopen <- getShpOpenData(Admin_shp)
			shpf <- shpfopen[[2]]
			ids <- as.numeric(tclvalue(tcl(adminVar.tab1, 'current')))+1
			selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(namePoly), ])
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

	#############################
	tkbind(chkSpaAvrg.tab4, "<Button-1>", function(){
		if(tclvalue(spatAverage) == '0'){
			tclvalue(fileORdir) <- 'File to save extracted data'
			tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = TRUE))
		}else{
			if((tclvalue(area_type) == 'Rectangle' | tclvalue(area_type) == 'Polygon') & tclvalue(ChoixOutType) == '1'){
				tclvalue(fileORdir) <- 'Directory to save extracted data'
				tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = FALSE))
			}else{
				tclvalue(fileORdir) <- 'File to save extracted data'
				tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = TRUE))
			}
		}
	})

	#############################
	tkbind(outTypeRadio1, "<Button-1>", function(){
		if(tclvalue(spatAverage) == '0'){
			if(tclvalue(area_type) == 'Rectangle' | tclvalue(area_type) == 'Polygon'){
				tclvalue(fileORdir) <- 'Directory to save extracted data'
				tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = FALSE))
			}else{
				tclvalue(fileORdir) <- 'File to save extracted data'
				tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = TRUE))
			}
		}
	})

	#############################
	tkbind(outTypeRadio2, "<Button-1>", function(){
		tclvalue(fileORdir) <- 'File to save extracted data'
		tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = TRUE))
	})

	#############################
	tkbind(outTypeRadio3, "<Button-1>", function(){
		tclvalue(fileORdir) <- 'File to save extracted data'
		tkconfigure(bfl2sav.tab4, command = function() fileORdir2Save(file.save1, isFile = TRUE))
	})

	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)

	######
	return(cmd.frame)
}

#######################################################################################################

previewWin <- function(parent.win, states, shpL){
	listOpenFiles <- openFile_ttkcomboList()
	tt <- tktoplevel()
	frA <- tkframe(tt, relief = "raised", borderwidth = 2)
	frB <- tkframe(tt)

	########
	frA1 <- ttklabelframe(frA, text = 'Multiple Points', relief = 'groove', borderwidth = 2)

	coordsfiles <- tclVar()
	coordsfrom <- tclVar('crd')

	crdfrm1 <- tkradiobutton(frA1, variable = coordsfrom, value = "crd", text = "From coordinate file", anchor = 'w', justify = 'left', state = states[1])
	crdfrm2 <- tkradiobutton(frA1, variable = coordsfrom, value = "cdt", text = "From CDT data", anchor = 'w', justify = 'left', state = states[1])
	cbmltpts <- ttkcombobox(frA1, values = unlist(listOpenFiles), textvariable = coordsfiles, state = states[1], width = w.opfiles-5)
	btmltpts <- tkbutton(frA1, text = "...", state = states[1])

	tkconfigure(btmltpts, command = function(){
		tkdelete(textObj, "0.0", "end")
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(coordsfiles) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cbmltpts, values = unlist(listOpenFiles), textvariable = coordsfiles)
			
			crds <- dat.opfiles[[2]]
			if(tclvalue(coordsfrom) == 'crd') crds <- crds[, c(1, 3, 4), drop = FALSE]
			if(tclvalue(coordsfrom) == 'cdt') crds <- t(crds[1:3, -1, drop = FALSE])
			for(i in 1:nrow(crds))	tkinsert(textObj, "end", paste(crds[i, 1], crds[i, 2], crds[i, 3], "\n"))
		}
	})

	tkgrid(crdfrm1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
 	tkgrid(crdfrm2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cbmltpts, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(btmltpts, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(crdfrm1, 'The coordinates come from a coordinate file')
	status.bar.display(crdfrm1, TextOutputVar, 'The coordinates come from a coordinate file')
	infobulle(crdfrm2, 'The coordinates come from a CDT data file')
	status.bar.display(crdfrm2, TextOutputVar, 'The coordinates come from a CDT data file')
	infobulle(cbmltpts, 'Choose the file in the list')
	status.bar.display(cbmltpts, TextOutputVar, 'File containing the ids and coordinates of points to be extracted')
	infobulle(btmltpts, 'Browse file if not listed')
	status.bar.display(btmltpts, TextOutputVar, 'Browse file if not listed')

	tkgrid(frA1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	########
	tkbind(cbmltpts, "<<ComboboxSelected>>", function(){
		tkdelete(textObj, "0.0", "end")
		crds <- getStnOpenData(coordsfiles)
		if(tclvalue(coordsfrom) == 'crd') crds <- crds[, c(1, 3, 4), drop = FALSE]
		if(tclvalue(coordsfrom) == 'cdt') crds <- t(crds[1:3, -1, drop = FALSE])
		for(i in 1:nrow(crds))	tkinsert(textObj, "end", paste(crds[i, 1], crds[i, 2], crds[i, 3], "\n"))
 	})

	########
	frA2 <- ttklabelframe(frA, text = 'Multiple Polygons', relief = 'groove', borderwidth = 2)

	btmpoly <- tkbutton(frA2, text = 'Get All Polygons', state = states[2])

	tkconfigure(btmpoly, command = function(){
		tkdelete(textObj, "0.0", "end")
		shpf <- getShpOpenData(shpL[[2]])
		if(!is.null(shpf)){
			dat <- shpf[[2]]@data
			adminN <- as.character(dat[,as.numeric(tclvalue(tcl(shpL[[1]],'current')))+1])
			shpAttr <- levels(as.factor(adminN))
			for(i in 1:length(shpAttr))	tkinsert(textObj, "end", paste(shpAttr[i], "\n", sep = ''))
		}
	})

	tkgrid(btmpoly, sticky = 'we', rowspan = 1, columnspan = 1, padx = 10, pady = 1, ipadx = 1, ipady = 1)

	infobulle(btmpoly, 'Get all the polygons from the shapefile')
	status.bar.display(btmpoly, TextOutputVar, 'Get all the polygons from the shapefile')

	tkgrid(frA2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 20, ipady = 1)

	########
	frA3 <- tkframe(frA, relief = 'groove', borderwidth = 2)

	yscr <- tkscrollbar(frA3, repeatinterval = 4, command = function(...) tkyview(textObj,...))
	textObj <- tktext(frA3, bg = "white", yscrollcommand = function(...) tkset(yscr,...), wrap = "none", height = 5, width = w.opfiles+5) #

	tkgrid(textObj, yscr)
	tkgrid.configure(yscr, sticky = "ns")
	tkgrid.configure(textObj, sticky = 'nswe')

	tkgrid(frA3, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	########################
	btOK <- tkbutton(frB, text = "OK")
	btCA <- tkbutton(frB, text = "Cancel")

	tkgrid(btOK, row = 0, column = 0, padx = 5, pady = 5, ipadx = 5, sticky = 'w')
	tkgrid(btCA, row = 0, column = 1, padx = 5, pady = 5, sticky = 'e')

	########################

	ret.params <- list(win = tt, textObj = textObj)

	tkconfigure(btOK, command = function(){
		retvars <- tclvalue(tkget(textObj, "0.0", "end"))
		assign('multiptspoly', retvars, envir = EnvMultiPP)
		tkdestroy(tt)
		tkfocus(parent.win)
		ret.params <<- NULL
	})

	tkconfigure(btCA, command = function(){
		tkdestroy(tt)
		tkfocus(parent.win)
		ret.params <<- NULL
	})

	tkgrid(frA, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frB, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#########################
	tkwm.withdraw(tt)
	tcl('update')
	tkwm.geometry(tt, '+5+15')
	tkwm.transient(tt, parent.win)
	tkwm.title(tt, 'Multiple Points/Polygons')
	tkwm.deiconify(tt)

	tkfocus(parent.win)
	tkbind(tt, "<Destroy>", function(){
		tkfocus(parent.win)
		ret.params <<- NULL
	})
	return(ret.params)
}



PlotCDTDataFormatCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(24)
		hscrlwin <- h.scale(30)
		wPreview <- w.scale(21)
		largeur <- as.integer(w.scale(27)/sfont0)
	}else{
		wscrlwin <- w.scale(24)  
		hscrlwin <- h.scale(27) 
		wPreview <- w.scale(22)
		largeur <- as.integer(w.scale(21)/sfont0)
	}

	PlotCDTdata <- fromJSON(file.path(apps.dir, 'init_params', 'Plot_CDT_Data.json'))

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	plotBut.cmd <- tkframe(cmd.frame)
	tkgrid(tknote.cmd, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2)
	tkgrid(plotBut.cmd, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1)
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "General")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Options")
	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	
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
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(PlotCDTdata$cdt$cdt.freq, 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])
	file.stnfl <- tclVar(PlotCDTdata$cdt$cdt.file)
	unit_sym <- tclVar(PlotCDTdata$cdt$cdt.uni)
	file.plotShp <- tclVar(PlotCDTdata$shp)

	idate_yrs <- tclVar(PlotCDTdata$date$year)
	idate_mon <- tclVar(PlotCDTdata$date$mon)
	idate_day <- tclVar(PlotCDTdata$date$day)
	dayLabTab1_Var <- tclVar(PlotCDTdata$date$day.label)

	##############
	frameStn <- ttklabelframe(subfr1, text = "Station data file", relief = 'groove')

	combPrd.tab1 <- ttkcombobox(frameStn, values = cb.periodVAL, textvariable = file.period, width = largeur)
	combStnfl.tab1 <- ttkcombobox(frameStn, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur)
	btStnfl.tab1 <- tkbutton(frameStn, text = "...") 

	labDate.tab1 <- tklabel(frameStn, text = "Date", anchor = 'e', justify = 'right')
	yrsLab.tab1 <- tklabel(frameStn, text = 'Year', anchor = 'w', justify = 'left')
	monLab.tab1 <- tklabel(frameStn, text = 'Month', anchor = 'w', justify = 'left')
	dayLab.tab1 <- tklabel(frameStn, text = tclvalue(dayLabTab1_Var), textvariable = dayLabTab1_Var, anchor = 'w', justify = 'left')
	yrs1.tab1 <- tkentry(frameStn, width = 4, textvariable = idate_yrs, justify = "left")
	mon1.tab1 <- tkentry(frameStn, width = 4, textvariable = idate_mon, justify = "left")
	day1.tab1 <- tkentry(frameStn, width = 4, textvariable = idate_day, justify = "left")

	unitLab.tab1 <- tklabel(frameStn, text = 'Units', anchor = 'e', justify = 'right')
	unitEd.tab1 <- tkentry(frameStn, width = 8, textvariable = unit_sym, justify = "left")

	#######################
	tkconfigure(btStnfl.tab1, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]] 
			tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(combStnfl.tab1, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(combShp.tab1, values = unlist(listOpenFiles), textvariable = file.plotShp)
		}else return(NULL)
	})

	###############
	tkgrid(combPrd.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(combStnfl.tab1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(btStnfl.tab1, row = 1, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(yrsLab.tab1, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(monLab.tab1, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(dayLab.tab1, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(labDate.tab1, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(yrs1.tab1, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(mon1.tab1, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(day1.tab1, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(unitLab.tab1, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(unitEd.tab1, row = 4, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	infobulle(combStnfl.tab1, 'Choose the station data in the list')
	status.bar.display(combStnfl.tab1, TextOutputVar, 'Choose the file containing the station data')
	infobulle(btStnfl.tab1, 'Browse file if not listed')
	status.bar.display(btStnfl.tab1, TextOutputVar, 'Browse file if not listed')
	infobulle(unitEd.tab1, 'Unit to display on colorscale')
	status.bar.display(unitEd.tab1, TextOutputVar, 'Unit to display on colorscale')
	
	##############
	frameShp <- ttklabelframe(subfr1, text = "Shapefiles for boundary", relief = 'groove')

	combShp.tab1 <- ttkcombobox(frameShp, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur) 
	btShp.tab1 <- tkbutton(frameShp, text = "...") 

	###############
	tkconfigure(btShp.tab1, command = function(){
		shp.opfiles <- getOpenShp(main.win, all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'shp'
			AllOpenFilesData[[nopf+1]] <<- shp.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.plotShp) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(combStnfl.tab1, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(combShp.tab1, values = unlist(listOpenFiles), textvariable = file.plotShp)
		}
	})

	###############
	tkgrid(combShp.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(btShp.tab1, row = 0, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	infobulle(combShp.tab1, 'Choose the file containing the ESRI shapefiles')
	status.bar.display(combShp.tab1, TextOutputVar, 'Choose the file containing the ESRI shapefiles')
	infobulle(btShp.tab1, 'Browse file if not listed')
	status.bar.display(btShp.tab1, TextOutputVar, 'Browse file if not listed')

	#############################
	tkgrid(frameStn, row = 0, column = 0, sticky = 'we')
	tkgrid(frameShp, row = 1, column = 0, sticky = 'we')
	
	############################
	tkbind(combPrd.tab1, "<<ComboboxSelected>>", function(){
		if(tclvalue(file.period) == 'Daily data'){
			tclvalue(dayLabTab1_Var) <- 'Day'
			tkconfigure(day1.tab1, state = 'normal')
		}
		if(tclvalue(file.period) == 'Dekadal data'){
			tclvalue(dayLabTab1_Var) <- 'Dek'
			tkconfigure(day1.tab1, state = 'normal')
		}
		if(tclvalue(file.period) == 'Monthly data'){
			tkconfigure(day1.tab1, state = 'disabled')
		}
	})		
	
	#######################################################################################################

	#Tab2	
	frTab2 <- tkframe(cmd.tab2) #,relief = 'sunken', bd = 2
	tkgrid(frTab2, padx = 5, pady = 5, ipadx = 2, ipady = 2)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)

	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr2, 0, weight = 1)

	###############

	nb.color <- tclVar(PlotCDTdata$color.opt$nb.color)
	preset.color <- tclVar(PlotCDTdata$color.opt$preset.color)
	reverse.color <- tclVar(PlotCDTdata$color.opt$reverse.color)
	custom.color <- tclVar(PlotCDTdata$color.opt$custom.color)
	custom.level <- tclVar(PlotCDTdata$color.opt$custom.level)

	###############

	labPresetCol.tab2 <- tklabel(subfr2, text = 'Presets colorkey', anchor = 'w', justify = 'left')
	combPresetCol.tab2 <- ttkcombobox(subfr2, values = c('tim.colors', 'rainbow', 'heat.colors', 'cm.colors', 'topo.colors', 'terrain.colors'), textvariable = preset.color, width = 13)
	nbPresetCol.tab2 <- tkentry(subfr2, width = 3, textvariable = nb.color, justify = "left")

	labRevCol.tab2 <- tklabel(subfr2, text = 'Reverse', anchor = 'e', justify = 'right')
	chkRevCol.tab2 <- tkcheckbutton(subfr2, variable = reverse.color, anchor = 'w', justify = 'left')

	previewPresetCol.tab2 <- tkcanvas(subfr2, width = wPreview, height = 20, bg = 'white')

	chkCustoCol.tab2 <- tkcheckbutton(subfr2, variable = custom.color, text = 'User customized  colorkey', anchor = 'w', justify = 'left')
	butCustoCol.tab2 <- tkbutton(subfr2, text = "Custom", state = 'disabled')

	chkCustoLev.tab2 <- tkcheckbutton(subfr2, variable = custom.level, text = 'User customized  levels', anchor = 'w', justify = 'left')
	butCustoLev.tab2 <- tkbutton(subfr2, text = "Custom", state = 'disabled')

	sep1.tab2 <- ttkseparator(subfr2)
	sep2.tab2 <- ttkseparator(subfr2)
	sep3.tab2 <- ttkseparator(subfr2)

	#####
	tkgrid(labPresetCol.tab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(combPresetCol.tab2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(nbPresetCol.tab2, row = 0, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(labRevCol.tab2, row = 1, column = 2, sticky = 'e', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chkRevCol.tab2, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep1.tab2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)
	tkgrid(previewPresetCol.tab2, row = 3, column = 0, sticky = 'w', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
	tkgrid(sep2.tab2, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)
	tkgrid(chkCustoCol.tab2, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(butCustoCol.tab2, row = 5, column = 4, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep3.tab2, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)
	tkgrid(chkCustoLev.tab2, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(butCustoLev.tab2, row = 7, column = 4, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(combPresetCol.tab2, 'Predefined color palettes')
	status.bar.display(combPresetCol.tab2, TextOutputVar, 'Predefined color palettes')
	infobulle(nbPresetCol.tab2, 'Number of color levels to be in the palette')
	status.bar.display(nbPresetCol.tab2, TextOutputVar, 'Number of color levels to be in the palette')
	infobulle(chkRevCol.tab2, 'Reverse the color palettes')
	status.bar.display(chkRevCol.tab2, TextOutputVar, 'Reverse the color palettes')

	########################
	##Preview Color
	nkol <- as.numeric(PlotCDTdata$color.opt$nb.color)
	funkol <- match.fun(PlotCDTdata$color.opt$preset.color)
	kolor <- getGradientColor(funkol(nkol), 0:wPreview)
	tkdelete(previewPresetCol.tab2, 'gradlines0')
	for(i in 0:wPreview) tkcreate(previewPresetCol.tab2, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')

	tkbind(combPresetCol.tab2, "<<ComboboxSelected>>", function(){
		n <- as.numeric(tclvalue(nb.color))
		colFun <- match.fun(tclvalue(preset.color))
		listCol <- colFun(n)
		if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
		kolor <- getGradientColor(listCol, 0:wPreview)
		tkdelete(previewPresetCol.tab2, 'gradlines0')
		for(i in 0:wPreview) tkcreate(previewPresetCol.tab2, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
	})

	#reverse
	tkbind(chkRevCol.tab2, "<Button-1>", function(){
		if(tclvalue(custom.color) == '0'){
			n <- as.numeric(tclvalue(nb.color))
			colFun <- match.fun(tclvalue(preset.color))
			listCol <- colFun(n)
			if(tclvalue(reverse.color) == '0') listCol <- rev(listCol)
			kolor <- getGradientColor(listCol, 0:wPreview)
			tkdelete(previewPresetCol.tab2, 'gradlines0')
			for(i in 0:wPreview) tkcreate(previewPresetCol.tab2, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
		}
	})
	
	########################
	##Customized color	
	tkbind(chkCustoCol.tab2, "<Button-1>", function(){
		if(tclvalue(custom.color) == '0') tkconfigure(butCustoCol.tab2, state = 'normal')
		else tkconfigure(butCustoCol.tab2, state = 'disabled')
	})

	listCol <- NULL	
	tkconfigure(butCustoCol.tab2, command = function(){
		listCol <<- createColorkey(main.win, listCol)
		if(!is.null(listCol) & length(listCol) > 0){
			kolor <- getGradientColor(listCol, 0:wPreview)
			tkdelete(previewPresetCol.tab2, 'gradlines0')
			for(i in 0:wPreview) tkcreate(previewPresetCol.tab2, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
		}
	})

	########################
	##Customized level	
	tkbind(chkCustoLev.tab2, "<Button-1>", function(){
		if(tclvalue(custom.level) == '0') tkconfigure(butCustoLev.tab2, state = 'normal')
		else tkconfigure(butCustoLev.tab2, state = 'disabled')
	})
	
	atLev <- NULL
	# initLev <- NULL
	tkconfigure(butCustoLev.tab2, command = function(){
		# if(is.null(initLev) & is.null(atLev)){
		if(is.null(atLev)){
			donne <- getCDTdata(file.stnfl, file.period)
			donne <- getCDTdata1Date(donne, tclvalue(idate_yrs), tclvalue(idate_mon), tclvalue(idate_day))
			if(!is.null(donne)){
				atLev <- pretty(donne$z)
				# initLev <<- 1
			}
		}
		atLev <<- customLevels(main.win, atLev)
	})
	
	#######################################################################################################
	
	plotDataBut <- tkbutton(plotBut.cmd, text = "Plot Data")

	tkgrid(plotDataBut, row = 0, column = 0, sticky = 'e', padx = 5, pady = 5)

	#################
	
	notebookTab <- NULL
	#######

	tkconfigure(plotDataBut, command = function(){
		if(tclvalue(custom.color) == '0' | length(listCol) == 0){
			n <- as.numeric(tclvalue(nb.color))
			if(is.na(n)) n <- 10
			colFun <- match.fun(tclvalue(preset.color))
			listCol <- colFun(n)
			if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
		}

		donne <- getCDTdata(file.stnfl, file.period)
		donne <- getCDTdata1Date(donne, tclvalue(idate_yrs), tclvalue(idate_mon), tclvalue(idate_day))

		if(tclvalue(custom.level) == '0' | length(atLev) == 0){
			if(!is.null(donne)) atLev <- pretty(donne$z)
		}
		shpf <- getShpOpenData(file.plotShp)[[2]]
		
		imgContainer <- displayCDTdata(tknotes, notebookTab, donne, atLev, listCol, shpf, tclvalue(unit_sym))
		if(!is.null(imgContainer)){
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab, AllOpenTabType, AllOpenTabData)
			notebookTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})	

	
	#######################################################################################################
	
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}


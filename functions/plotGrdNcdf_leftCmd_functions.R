PlotGriddedNcdfCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(24)
		hscrlwin <- h.scale(25)
		wPreview <- w.scale(21)
		largeur <- as.integer(w.scale(27)/sfont0)
	}else{
		wscrlwin <- w.scale(24)
		hscrlwin <- h.scale(23)
		wPreview <- w.scale(22)
		largeur <- as.integer(w.scale(21)/sfont0)
	}

	PlotGrdNcdf <- fromJSON(file.path(apps.dir, 'init_params', 'Plot_Gridded_NetCDF.json'))

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	plotBut.cmd <- tkframe(cmd.frame)
	tkgrid(tknote.cmd, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2)
	tkgrid(plotBut.cmd, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1)
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)
	tkgrid.rowconfigure(tknote.cmd, 0, weight = 1)
	tkgrid.columnconfigure(plotBut.cmd, 0, weight = 1)

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
	# tkgrid.columnconfigure(subfr1, 0, weight = 1)
	# otra? taille fixe ny wscrlwin sy hscrlwin ts mety resizable 

	##############
	file.netcdf <- tclVar(PlotGrdNcdf$ncdf$nc.file)
	unit_sym <- tclVar(PlotGrdNcdf$ncdf$nc.unit)
	file.plotShp <- tclVar(PlotGrdNcdf$shp)
	blankVal <- tclVar(PlotGrdNcdf$blank)

	##############
	frameNcdf <- ttklabelframe(subfr1, text = "NetCDF data file", relief = 'groove')

	combNetcdf.tab1 <- ttkcombobox(frameNcdf, values = unlist(listOpenFiles), textvariable = file.netcdf, width = largeur)
	btNetcdf.tab1 <- tkbutton(frameNcdf, text = "...") 
	unitLab.tab1 <- tklabel(frameNcdf, text = 'Units', anchor = 'e', justify = 'right')
	unitEd.tab1 <- tkentry(frameNcdf, width = 8, textvariable = unit_sym, justify = "left")

	#######################
	tkconfigure(btNetcdf.tab1, command = function(){
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]] 
			tclvalue(file.netcdf) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(combNetcdf.tab1, values = unlist(listOpenFiles), textvariable = file.netcdf)
			tkconfigure(combShp.tab1, values = unlist(listOpenFiles), textvariable = file.plotShp)
		}else return(NULL)
	})

	#######################
	tkgrid(combNetcdf.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(btNetcdf.tab1, row = 0, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(unitLab.tab1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(unitEd.tab1, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	infobulle(combNetcdf.tab1, 'Choose a NetCDF data in the list')
	status.bar.display(combNetcdf.tab1, TextOutputVar, 'Choose the file containing the NetCDF data')
	infobulle(btNetcdf.tab1, 'Browse file if not listed')
	status.bar.display(btNetcdf.tab1, TextOutputVar, 'Browse file if not listed')
	infobulle(unitEd.tab1, 'Unit to display on colorscale')
	status.bar.display(unitEd.tab1, TextOutputVar, 'Unit to display on colorscale')
	
	##############
	frameShp <- ttklabelframe(subfr1, text = "Shapefiles for boundary", relief = 'groove')
	
	combShp.tab1 <- ttkcombobox(frameShp, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur) 
	btShp.tab1 <- tkbutton(frameShp, text = "...") 
	cbBlank.tab1 <- tkcheckbutton(subfr1, variable = blankVal, text = 'Blank grid', anchor = 'w', justify = 'left')

	##############
	tkconfigure(btShp.tab1, command = function(){
		shp.opfiles <- getOpenShp(main.win, all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'shp'
			AllOpenFilesData[[nopf+1]] <<- shp.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.plotShp) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(combNetcdf.tab1, values = unlist(listOpenFiles), textvariable = file.netcdf)
			tkconfigure(combShp.tab1, values = unlist(listOpenFiles), textvariable = file.plotShp)
		}
	})
	
	###########
	tkgrid(combShp.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(btShp.tab1, row = 0, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	infobulle(combShp.tab1, 'Choose the file containing the ESRI shapefiles')
	status.bar.display(combShp.tab1, TextOutputVar, 'Choose the file containing the ESRI shapefiles')
	infobulle(btShp.tab1, 'Browse file if not listed')
	status.bar.display(btShp.tab1, TextOutputVar, 'Browse file if not listed')
	infobulle(cbBlank.tab1, 'Blank grid outside the country boundaries or over ocean')
	status.bar.display(cbBlank.tab1, TextOutputVar, 'Blank grid outside the country boundaries  or over ocean given by the shapefile')
	
	#############################
	tkgrid(frameNcdf, row = 0, column = 0, sticky = 'we', pady = 2)
	tkgrid(frameShp, row = 1, column = 0, sticky = 'we', pady = 2)
	tkgrid(cbBlank.tab1, row = 3, column = 0, sticky = 'we', ipady = 2)
	tkgrid.columnconfigure(frameNcdf, 0, weight = 1)
	tkgrid.columnconfigure(frameShp, 0, weight = 1)
	tkgrid.columnconfigure(cbBlank.tab1, 0, weight = 1)

	#######################################################################################################

	#Tab2	
	frTab2 <- tkframe(cmd.tab2)
	tkgrid(frTab2, padx = 5, pady = 5, ipadx = 2, ipady = 2)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)

	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin)
	# tkgrid.columnconfigure(subfr2, 0, weight = 1)

	nb.color <- tclVar(PlotGrdNcdf$color.opt$nb.color)
	preset.color <- tclVar(PlotGrdNcdf$color.opt$preset.color)
	reverse.color <- tclVar(PlotGrdNcdf$color.opt$reverse.color)
	custom.color <- tclVar(PlotGrdNcdf$color.opt$custom.color)
	custom.level <- tclVar(PlotGrdNcdf$color.opt$custom.level)

	########################
	labPresetCol.tab2 <- tklabel(subfr2, text = 'Presets colorkey', anchor = 'w', justify = 'left')
	combPresetCol.tab2 <- ttkcombobox(subfr2, values = c('tim.colors', 'rainbow', 'heat.colors', 'cm.colors', 'topo.colors', 'terrain.colors'), textvariable = preset.color, width = 13)
	nbPresetCol.tab2 <- tkentry(subfr2, width = 3, textvariable = nb.color, justify = "left")

	labRevCol.tab2 <- tklabel(subfr2, text = 'Reverse', anchor = 'e', justify = 'right')
	chkRevCol.tab2 <- tkcheckbutton(subfr2, variable = reverse.color, anchor = 'w', justify = 'left')

	sep1.tab2 <- ttkseparator(subfr2)
	previewPresetCol.tab2 <- tkcanvas(subfr2, width = wPreview, height = 20, bg = 'white')

	sep2.tab2 <- ttkseparator(subfr2)
	chkCustoCol.tab2 <- tkcheckbutton(subfr2, variable = custom.color, text = 'User customized  colorkey', anchor = 'w', justify = 'left')
	butCustoCol.tab2 <- tkbutton(subfr2, text = "Custom", state = 'disabled')

	sep3.tab2 <- ttkseparator(subfr2)
	chkCustoLev.tab2 <- tkcheckbutton(subfr2, variable = custom.level, text = 'User customized  levels', anchor = 'w', justify = 'left')
	butCustoLev.tab2 <- tkbutton(subfr2, text = "Custom", state = 'disabled')

	########################
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
	nkol <- as.numeric(PlotGrdNcdf$color.opt$nb.color)
	funkol <- match.fun(PlotGrdNcdf$color.opt$preset.color)
	kolor <- getGradientColor(funkol(nkol), 0:wPreview)
	tkdelete(previewPresetCol.tab2, 'gradlines0')
	for(i in 0:wPreview) tkcreate(previewPresetCol.tab2, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')

	tkbind(combPresetCol.tab2,"<<ComboboxSelected>>", function(){
		n <- as.numeric(tclvalue(nb.color))
		colFun <- match.fun(tclvalue(preset.color))
		listCol <- colFun(n)
		if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
		kolor <- getGradientColor(listCol, 0:wPreview)
		tkdelete(previewPresetCol.tab2, 'gradlines0')
		for(i in 0:wPreview) tkcreate(previewPresetCol.tab2, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
	})

	#reverse
	tkbind(chkRevCol.tab2,"<Button-1>", function(){
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
	tkbind(chkCustoCol.tab2,"<Button-1>", function(){
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
	tkbind(chkCustoLev.tab2,"<Button-1>", function(){
		if(tclvalue(custom.level) == '0') tkconfigure(butCustoLev.tab2, state = 'normal')
		else tkconfigure(butCustoLev.tab2, state = 'disabled')
	})
	
	atLev <- NULL
	tkconfigure(butCustoLev.tab2, command = function(){
		if(is.null(atLev)){
			donne <- getNcdfOpenData(file.netcdf)[[2]]
			if(!is.null(donne)){
				atLev <- pretty(donne$value)
			}
		}
		atLev <<- customLevels(main.win, atLev)
	})
	
	#######################################################################################################
	
	notebookTab <- NULL
	#######

	plotDataBut <- tkbutton(plotBut.cmd, text = "Plot Data")
	
	tkconfigure(plotDataBut, command = function(){
		if(tclvalue(custom.color) == '0' | length(listCol) == 0){
			n <- as.numeric(tclvalue(nb.color))
			if(is.na(n)) n <- 10
			colFun <- match.fun(tclvalue(preset.color))
			listCol <- colFun(n)
			if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
		}

		donne <- getNcdfOpenData(file.netcdf)

		if(tclvalue(custom.level) == '0' | length(atLev) == 0){
			if(!is.null(donne)) atLev <- pretty(donne[[2]]$value)
		}
		shpf <- getShpOpenData(file.plotShp)[[2]]
		if(tclvalue(blankVal) == '1' & is.null(shpf)){
			InsertMessagesTxt(main.txt.out, 'Need ESRI shapefile for blanking', format = TRUE)
		}else{
			imgContainer <- displayNetCDFdata(tknotes, notebookTab, donne, atLev, listCol, shpf, tclvalue(unit_sym), tclvalue(blankVal))
			if(!is.null(imgContainer)){
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab, AllOpenTabType, AllOpenTabData)
				notebookTab <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		}
	})

	tkgrid(plotDataBut, row = 0, column = 0, sticky = 'e', padx = 5, pady = 5)
	
	#######################################################################################################
	
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	tkgrid.columnconfigure(cmd.frame, 1, weight = 1)
	tkgrid.rowconfigure(cmd.frame, 0, weight = 1)

	######
	return(cmd.frame)
}



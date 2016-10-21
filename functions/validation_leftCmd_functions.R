
ValidationPanelCmd <- function(clim.var){
	listOpenFiles <- openFile_ttkcomboList()
	largeur <- as.integer(w.scale(21)/sfont0)
	wncdf_ff <- as.integer(w.scale(14)/sfont0)
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(20)
		hscrlwin <- h.scale(31.5)
		wttkcombo <- as.integer(as.numeric(w.scale(20)*0.95)/9)
	}else{
		wscrlwin <- w.scale(24)
		hscrlwin <- h.scale(38.5)
		wttkcombo <- as.integer(as.numeric(w.scale(24)*0.95)/9)
	}

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "General")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Select Stations")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Validation")

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

	##############
	frameStn <- ttklabelframe(subfr1, text = "Gauge validation data file", relief = 'groove')
	frameNcdf <- ttklabelframe(subfr1, text = "NetCDF files", relief = 'groove')
	frameDirSav <- ttklabelframe(subfr1, text = "Directory to save result", relief = 'groove')

	#######################
	file.period <- tclVar()
	tclvalue(file.period) <- 'Dekadal data'
	file.stnfl <- tclVar()

	combPrd.tab1 <- ttkcombobox(frameStn, values = c('Daily data', 'Dekadal data', 'Monthly data'), textvariable = file.period)
	combStnfl.tab1 <- ttkcombobox(frameStn, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur)
	btStnfl.tab1 <- tkbutton(frameStn, text = "...")

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
			tkconfigure(combgrdCDF.tab1, values = unlist(listOpenFiles), textvariable = file.grdCDF)
			tkconfigure(combShp.tab2, values = unlist(listOpenFiles), textvariable = file.plotShp)
		}else return(NULL)
	})

	#######################

	tkgrid(combPrd.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(combStnfl.tab1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(btStnfl.tab1, row = 1, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	infobulle(combStnfl.tab1, 'Choose the station data in the list')
	status.bar.display(combStnfl.tab1, TextOutputVar, 'Choose the file containing the station data in CDT format')
	infobulle(btStnfl.tab1, 'Browse file if not listed')
	status.bar.display(btStnfl.tab1, TextOutputVar, 'Browse file if not listed')

	#######################
	if(clim.var == 'RR') init.ff <- "rfe%s_%s-dk%s.nc"
	if(clim.var == 'TT') init.ff <- "tmax_%s%s%s.nc"
	
	dirNetCDF <- tclVar()
	netCDFff <- tclVar(init.ff)
	file.grdCDF <- tclVar()

	labNcdir.tab1 <- tklabel(frameNcdf, text = "Directory of NetCDF files", anchor = 'w', justify = 'left')
	dirCDF.tab1 <- tkentry(frameNcdf, textvariable = dirNetCDF, width = largeur) #
	bdirCDF.tab1 <- tkbutton(frameNcdf, text = "...")
	cap1.tab1 <- tklabel(frameNcdf, text = "NetCDF file format", anchor = 'e', justify = 'right')
	netCDFff.tab1 <- tkentry(frameNcdf, textvariable = netCDFff, width = wncdf_ff)
	labRFE.tab1 <- tklabel(frameNcdf, text = "NetCDF's sample file", anchor = 'w', justify = 'left')
	combgrdCDF.tab1 <- ttkcombobox(frameNcdf, values = unlist(listOpenFiles), textvariable = file.grdCDF, width = largeur)
	btgrdCDF.tab1 <- tkbutton(frameNcdf, text = "...")

	#######################
	tkconfigure(bdirCDF.tab1, command = function(){
		dir4cdf <- tk_choose.dir(tclvalue(dirNetCDF), "")
		if(is.na(dir4cdf)) tclvalue(dirNetCDF)<-""
		else tclvalue(dirNetCDF) <- dir4cdf
	})

	tkconfigure(btgrdCDF.tab1, command = function(){
		fileopen <- tclvalue(tkgetOpenFile(initialdir = tclvalue(dirNetCDF), initialfile = "", filetypes="{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"))
		if(fileopen == "" | is.na(fileopen) | fileopen == "NA") return(NULL)
		nc.opfiles1 <- preview.data.nc(main.win, fileopen,"")
		nc.opfiles <- list(basename(fileopen), nc.opfiles1, fileopen)
		if(!is.null(nc.opfiles1)){
			tkinsert(all.opfiles, "end", basename(fileopen))
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdCDF) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(combStnfl.tab1, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(combgrdCDF.tab1, values = unlist(listOpenFiles), textvariable = file.grdCDF)
			tkconfigure(combShp.tab2, values = unlist(listOpenFiles), textvariable = file.plotShp)
		}else return(NULL)
	})

	#############################
	tkgrid(labNcdir.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dirCDF.tab1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bdirCDF.tab1, row = 1, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cap1.tab1, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(netCDFff.tab1, row = 2, column = 2, sticky = 'w', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(labRFE.tab1, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(combgrdCDF.tab1, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(btgrdCDF.tab1, row = 4, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(netCDFff.tab1, 'Enter the format of the NetCDF files names,\nexample: rfe1983_01-dk1.nc')
	status.bar.display(netCDFff.tab1, TextOutputVar, 'Enter the format of the NetCDF files names, example: rfe1983_01-dk1.nc')
	infobulle(combgrdCDF.tab1, 'Choose the file in the list')
	status.bar.display(combgrdCDF.tab1, TextOutputVar, 'File containing a sample of NetCDF data')
	infobulle(btgrdCDF.tab1, 'Browse file if not listed')
	status.bar.display(btgrdCDF.tab1, TextOutputVar, 'Browse file if not listed')

	#######################
	file.save1 <- tclVar()

	fl2sav.tab1 <- tkentry(frameDirSav, textvariable = file.save1, width = largeur) #
	bfl2sav.tab1 <- tkbutton(frameDirSav, text = "...")
	#######################

	tkconfigure(bfl2sav.tab1, command = function() fileORdir2Save(file.save1, isFile = FALSE))

	#############################

	tkgrid(fl2sav.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bfl2sav.tab1, row = 0, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(fl2sav.tab1, 'Enter the full path to the directory  to save result')
	status.bar.display(fl2sav.tab1, TextOutputVar, 'Enter the full path to the directory to save extracted data')
	infobulle(bfl2sav.tab1, 'Browse here the full path to the directory to save result')
	status.bar.display(bfl2sav.tab1, TextOutputVar, 'Browse here the full path to the directory to save extracted data')

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

	#####
	frameZoom <- ttklabelframe(subfr2, text = "ZOOM", relief = 'groove')

	btZoomP.tab2 <<- tkbutton(frameZoom, image = pikZoomPlus, relief = 'raised', bg = 'lightblue', state = 'normal')
	btZoomM.tab2 <<- tkbutton(frameZoom, image = pikZoomMinus, relief = 'raised', bg = 'lightblue', state = 'normal')
	btZoomRect.tab2 <<- tkbutton(frameZoom, image = pikZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
	btPanImg.tab2 <<- tkbutton(frameZoom, image = pikPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
	btRedraw.tab2 <- tkbutton(frameZoom, image = pikRedraw, relief = 'raised', state = 'disabled')
	btReset.tab2 <- tkbutton(frameZoom, image = pikReset, relief = 'raised')

	#######################
	tkgrid(btZoomP.tab2, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(btZoomM.tab2, row = 0, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(btZoomRect.tab2, row = 0, column = 2, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(btReset.tab2, row = 1, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(btRedraw.tab2, row = 1, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid(btPanImg.tab2, row = 1, column = 2, sticky = 'nswe', rowspan = 1, columnspan = 1)

	infobulle(btZoomP.tab2, 'Zoom In')
	status.bar.display(btZoomP.tab2, TextOutputVar, 'Zoom In')
	infobulle(btZoomM.tab2, 'Zoom Out')
	status.bar.display(btZoomM.tab2, TextOutputVar, 'Zoom Out')
	infobulle(btZoomRect.tab2, 'Zoom Area')
	status.bar.display(btZoomRect.tab2, TextOutputVar, 'Zoom Area')
	infobulle(btPanImg.tab2, 'Pan Tool')
	status.bar.display(btPanImg.tab2, TextOutputVar, 'Pan Tool')
	infobulle(btRedraw.tab2, 'Redraw Map')
	status.bar.display(btRedraw.tab2, TextOutputVar, 'Redraw Map')
	infobulle(btReset.tab2,' Zoom Reset')
	status.bar.display(btReset.tab2, TextOutputVar,' Zoom Reset')

	#######################

	frameDisp <- tkframe(subfr2)

	minlonRect <<- tclVar()
	maxlonRect <<- tclVar()
	minlatRect <<- tclVar()
	maxlatRect <<- tclVar()

	xx1 <<- tclVar()
	xx2 <<- tclVar()
	yy1 <<- tclVar()
	yy2 <<- tclVar()
	ZoomXYval0 <- NULL
	notebookTab <- NULL
	#######################

	bdisp.tab2 <- tkbutton(frameDisp, text = "Display Map")
	bselect.tab2 <<- tkbutton(frameDisp, text = "Select", relief = 'raised', bg = 'lightblue')

	minLab.tab2 <- tklabel(frameDisp, text = 'Min')
	maxLab.tab2 <- tklabel(frameDisp, text = 'Max')
	lonLab.tab2 <- tklabel(frameDisp, text = 'Lon', anchor = 'e', justify = 'right')
	latLab.tab2 <- tklabel(frameDisp, text = 'Lat', anchor = 'e', justify = 'right')
	minlon.tab2 <- tkentry(frameDisp, width = 4, textvariable = minlonRect, justify = "left", state = 'disabled')
	maxlon.tab2 <- tkentry(frameDisp, width = 4, textvariable = maxlonRect, justify = "left", state = 'disabled')
	minlat.tab2 <- tkentry(frameDisp, width = 4, textvariable = minlatRect, justify = "left", state = 'disabled')
	maxlat.tab2 <- tkentry(frameDisp, width = 4, textvariable = maxlatRect, justify = "left", state = 'disabled')

	#######################

	tkconfigure(bdisp.tab2, command = function(){
		donne <- getStnOpenData(file.stnfl)
		shpofile <- getShpOpenData(file.plotShp)
		if(!is.null(donne)){
			idStn <- as.character(donne[1, -1])
			lonStn <- as.numeric(donne[2, -1])
			latStn <- as.numeric(donne[3, -1])
			donne <- donne[1:3, -1]
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
		if(tclvalue(select_type) == 'Polygons' & plotOK){
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
			tclvalue(xx1) <<- lo1
			tclvalue(xx2) <<- lo2
			tclvalue(yy1) <<- la1
			tclvalue(yy2) <<- la2
			ZoomXYval <- as.numeric(c(tclvalue(xx1), tclvalue(xx2), tclvalue(yy1), tclvalue(yy2)))

			imgContainer <- displayMap4Validation(tknotes, donne, shpf, ZoomXYval, notebookTab)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab, AllOpenTabType, AllOpenTabData)
			notebookTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	#######################

	tkgrid(bdisp.tab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
	tkgrid(bselect.tab2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(minLab.tab2, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(maxLab.tab2, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(lonLab.tab2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(latLab.tab2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(minlon.tab2, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(maxlon.tab2, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(minlat.tab2, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
	tkgrid(maxlat.tab2, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)

	#######################

	frameSelect <- ttklabelframe(subfr2, text = "Selection Type", relief = 'groove')
	
	select_type <<- tclVar('All Stations')
	
	cbAreaType.tab2 <- ttkcombobox(frameSelect, values = c('All Stations', 'Rectangle', 'Polygons'), textvariable = select_type, width = wttkcombo)

	tkgrid(cbAreaType.tab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

	######
	tkbind(cbAreaType.tab2, "<<ComboboxSelected>>", function(){
		
		selectedPolygon <- NULL

		if(tclvalue(select_type) == 'All Stations'){
			tkconfigure(minlon.tab2, state = 'disabled')
			tkconfigure(maxlon.tab2, state = 'disabled')
			tkconfigure(minlat.tab2, state = 'disabled')
			tkconfigure(maxlat.tab2, state = 'disabled')
			tkconfigure(adminVar.tab2, state = 'disabled')
			tkconfigure(cbpolyType.tab2, state = 'disabled')
		}
		
		if(tclvalue(select_type) == 'Rectangle'){
			tkconfigure(minlon.tab2, state = 'normal')
			tkconfigure(maxlon.tab2, state = 'normal')
			tkconfigure(minlat.tab2, state = 'normal')
			tkconfigure(maxlat.tab2, state = 'normal')
			tkconfigure(adminVar.tab2, state = 'disabled')
			tkconfigure(cbpolyType.tab2, state = 'disabled')
		}

		##
		if(tclvalue(select_type) == 'Polygons'){
			tkconfigure(minlon.tab2, state = 'disabled')
			tkconfigure(maxlon.tab2, state = 'disabled')
			tkconfigure(minlat.tab2, state = 'disabled')
			tkconfigure(maxlat.tab2, state = 'disabled')
			tkconfigure(adminVar.tab2, state = 'normal')
			tkconfigure(cbpolyType.tab2, state = 'normal')
			if(tclvalue(namePoly) != ''){
				shpfopen <- getShpOpenData(file.plotShp)
				if(!is.null(shpfopen)){
					shpf <- shpfopen[[2]]
					ids <- as.numeric(tclvalue(tcl(adminVar.tab2, 'current')))+1
					selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(namePoly), ])
				}
			}
		}

		##
		tclvalue(minlonRect) <<- ''
		tclvalue(maxlonRect) <<- ''
		tclvalue(minlatRect) <<- ''
		tclvalue(maxlatRect) <<- ''
		tkconfigure(bselect.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')

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

	file.plotShp <- tclVar()
	Admin_var <- tclVar()
	namePoly <<- tclVar()

	combShp.tab2 <- ttkcombobox(frameShp, values = unlist(listOpenFiles), textvariable = file.plotShp, width = wttkcombo-4)
	btShp.tab2 <- tkbutton(frameShp, text = "...")
	attrLab.tab2 <- tklabel(frameShp, text = "Attribute field to be used and displayed", anchor = 'w', justify = 'left')
	adminVar.tab2 <<- ttkcombobox(frameShp, values='', textvariable = Admin_var, width = wttkcombo, state = 'disabled')
	cbpolyType.tab2 <- ttkcombobox(frameShp, values='', textvariable = namePoly, width = wttkcombo, state = 'disabled')

	tkgrid(combShp.tab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
	tkgrid(btShp.tab2, row = 0, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)
	tkgrid(attrLab.tab2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
	tkgrid(adminVar.tab2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 2)
	tkgrid(cbpolyType.tab2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 2)

	#######################
	tkconfigure(btShp.tab2, command = function(){
		shp.opfiles <- getOpenShp(main.win, all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'shp'
			AllOpenFilesData[[nopf+1]] <<- shp.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.plotShp) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(combShp.tab2, values = unlist(listOpenFiles), textvariable = file.plotShp)
			tkconfigure(combStnfl.tab1, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(combgrdCDF.tab1, values = unlist(listOpenFiles), textvariable = file.grdCDF)

			###
			shpf <- getShpOpenData(file.plotShp)
			dat <- shpf[[2]]@data
			shpAttr <- names(dat)
			tclvalue(Admin_var) <- shpAttr[1]
			adminN <- as.character(dat[, 1])
			name.poly <- levels(as.factor(adminN))
			tclvalue(namePoly) <- name.poly[1]

			tkconfigure(adminVar.tab2, values = shpAttr, textvariable = Admin_var)
			tkconfigure(cbpolyType.tab2, values = name.poly, textvariable = namePoly)
		}
	})

	#######################
	tkbind(combShp.tab2, "<<ComboboxSelected>>", function(){
		shpf <- getShpOpenData(file.plotShp)
		if(!is.null(shpf)){
			dat <- shpf[[2]]@data
			shpAttr <- names(dat)
			tclvalue(Admin_var) <- shpAttr[1]
			adminN <- as.character(dat[, as.numeric(tclvalue(tcl(adminVar.tab2, 'current')))+1])
			name.poly <- levels(as.factor(adminN))
		}else{
			shpAttr <- ''
			tclvalue(Admin_var) <- ''
			name.poly <- ''
			tclvalue(namePoly) <- ''
		}
		tkconfigure(adminVar.tab2, values = shpAttr, textvariable = Admin_var)
		tkconfigure(cbpolyType.tab2, values = name.poly, textvariable = namePoly)
	})

	########################
	tkbind(adminVar.tab2, "<<ComboboxSelected>>", function(){
		shpf <- getShpOpenData(file.plotShp)
		if(!is.null(shpf)){
			dat <- shpf[[2]]@data
			adminN <- as.character(dat[, as.numeric(tclvalue(tcl(adminVar.tab2, 'current')))+1])
			name.poly <- levels(as.factor(adminN))
		}else{
			name.poly <- ''
		}
		tclvalue(namePoly) <- name.poly[1]
		tkconfigure(cbpolyType.tab2, values = name.poly, textvariable = namePoly)
	})

	########################
	tkbind(cbpolyType.tab2, "<<ComboboxSelected>>", function(){
		if(tclvalue(namePoly) != ''){
			shpfopen <- getShpOpenData(file.plotShp)
			if(!is.null(shpfopen)){
				shpf <- shpfopen[[2]]
				ids <- as.numeric(tclvalue(tcl(adminVar.tab2, 'current')))+1
				selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(namePoly), ])
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

	tkgrid(frameZoom, row = 0, column = 0, sticky = 'we')
	tkgrid(frameDisp, row = 0, column = 1, sticky = 'we')
	tkgrid(frameSelect, row = 1, column = 0, sticky = 'we', columnspan = 2, pady = 5)
	tkgrid(frameShp, row = 2, column = 0, sticky = 'we', columnspan = 2)
	# tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', pady = 3)


	##########################
	tkconfigure(btReset.tab2, command = function(){
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

	tkbind(btReset.tab2, "<Button-1>", function(){
		tclvalue(pressButP) <<- 0
		tclvalue(pressButM) <<- 0
		tclvalue(pressButRect) <<- 0
		tclvalue(pressButDrag) <<- 0
		tclvalue(pressGetCoords) <<- 0
		tkconfigure(btZoomP.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomM.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomRect.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btPanImg.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(bselect.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
	})

	tkbind(btZoomP.tab2, "<Button-1>", function(){
		tclvalue(pressButP) <<- 1
		tclvalue(pressButM) <<- 0
		tclvalue(pressButRect) <<- 0
		tclvalue(pressButDrag) <<- 0
		tclvalue(pressGetCoords) <<- 0
		tkconfigure(btZoomP.tab2, relief = 'raised', bg = 'red', state = 'disabled')
		tkconfigure(btZoomM.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomRect.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btPanImg.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(bselect.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
	})

	tkbind(btZoomM.tab2, "<Button-1>", function(){
		tclvalue(pressButP) <<- 0
		tclvalue(pressButM) <<- 1
		tclvalue(pressButRect) <<- 0
		tclvalue(pressButDrag) <<- 0
		tclvalue(pressGetCoords) <<- 0
		tkconfigure(btZoomP.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomM.tab2, relief = 'raised', bg = 'red', state = 'disabled')
		tkconfigure(btZoomRect.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btPanImg.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(bselect.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
	})

	tkbind(btZoomRect.tab2, "<Button-1>", function(){
		tclvalue(pressButP) <<- 0
		tclvalue(pressButM) <<- 0
		tclvalue(pressButRect) <<- 1
		tclvalue(pressButDrag) <<- 0
		tclvalue(pressGetCoords) <<- 0
		tkconfigure(btZoomP.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomM.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomRect.tab2, relief = 'raised', bg = 'red', state = 'disabled')
		tkconfigure(btPanImg.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(bselect.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
	})

	tkbind(btPanImg.tab2, "<Button-1>", function(){
		tclvalue(pressButP) <<- 0
		tclvalue(pressButM) <<- 0
		tclvalue(pressButRect) <<- 0
		tclvalue(pressButDrag) <<- 1
		tclvalue(pressGetCoords) <<- 0
		tkconfigure(btZoomP.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomM.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomRect.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btPanImg.tab2, relief = 'raised', bg = 'red', state = 'disabled')
		tkconfigure(bselect.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
	})

	tkbind(bselect.tab2, "<Button-1>", function(){
		tclvalue(pressButP) <<- 0
		tclvalue(pressButM) <<- 0
		tclvalue(pressButRect) <<- 0
		tclvalue(pressButDrag) <<- 0
		tclvalue(pressGetCoords) <<- 1
		tkconfigure(btZoomP.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomM.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomRect.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btPanImg.tab2, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(bselect.tab2, relief = 'raised', bg = 'red', state = 'disabled')
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

	#####
	frameSeason <- ttklabelframe(subfr3, text = "Years & Season", relief = 'groove')

	#######################
	MonthsName <- format(ISOdate(2014, 1:12, 1), "%B")
	start_mois <- tclVar(MonthsName[1])
	end_mois <- tclVar(MonthsName[12])
	start_year <- tclVar('1990')
	end_year <- tclVar('2000')

	mon1Lab.tab3 <- tklabel(frameSeason, text = 'Start month', anchor = 'w', justify = 'left')
	mon2Lab.tab3 <- tklabel(frameSeason, text = 'End month', anchor = 'w', justify = 'left')
	cbChoixM1.tab3 <- ttkcombobox(frameSeason, values = MonthsName, textvariable = start_mois, width = 10) #
	cbChoixM2.tab3 <- ttkcombobox(frameSeason, values = MonthsName, textvariable = end_mois, width = 10) #
	years1Lab.tab3 <- tklabel(frameSeason, text = 'Start year', anchor = 'e', justify = 'right')
	years2Lab.tab3 <- tklabel(frameSeason, text = 'End year', anchor = 'w', justify = 'left')
	years1En.tab3 <- tkentry(frameSeason, width = 5, textvariable = start_year, justify = 'right')
	years2En.tab3 <- tkentry(frameSeason, width = 5, textvariable = end_year, justify = 'right')

	tkgrid(years1Lab.tab3, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(years1En.tab3, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(years2Lab.tab3, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(years2En.tab3, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(mon1Lab.tab3, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(cbChoixM1.tab3, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
	tkgrid(mon2Lab.tab3, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(cbChoixM2.tab3, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)

	infobulle(years1En.tab3, 'Start year to be used for validation')
	status.bar.display(years1En.tab3, TextOutputVar, 'Start year to be used for validation')
	infobulle(years2En.tab3, 'End year to be used for validation')
	status.bar.display(years2En.tab3, TextOutputVar, 'End year to be used for validation')

	#######################
	dataType <- tclVar('All Data')

	dataType.tab3 <- ttkcombobox(subfr3, values = c('All Data', 'Spatial Average'), textvariable = dataType)
	validate.tab3 <- tkbutton(subfr3, text = "EXECUTE")
	stats.tab3 <- tkbutton(subfr3, text = "Statistics")
	scatt.tab3 <- tkbutton(subfr3, text = "Gauge-RFE Plot")
	ecdf.tab3 <- tkbutton(subfr3, text = "CDF Plot")

	sep1.tab3 <- ttkseparator(subfr3)
	sep2.tab3 <- ttkseparator(subfr3)
	sep3.tab3 <- ttkseparator(subfr3)
	sep4.tab3 <- ttkseparator(subfr3)
	sep5.tab3 <- ttkseparator(subfr3)

	#############################
	tkgrid(frameSeason, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep1.tab3, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 3)
	tkgrid(tklabel(subfr3, text=' ',width = 6), row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dataType.tab3, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(tklabel(subfr3, text=' ',width = 6), row = 2, column = 5, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep2.tab3, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 3)
	tkgrid(validate.tab3, row = 4, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep3.tab3, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 3)
	tkgrid(stats.tab3, row = 6, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep4.tab3, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 3)
	tkgrid(scatt.tab3, row = 8, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep5.tab3, row = 9, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 3)
	tkgrid(ecdf.tab3, row = 10, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#############################

	outValiddata <- NULL
	tkconfigure(validate.tab3, command = function(){
		Inputs <- c(tclvalue(file.period), tclvalue(file.stnfl), tclvalue(dirNetCDF), tclvalue(netCDFff))
		if(is.null(EnvRainValidation$Inputs)){
			do_extr <- 1
			assign('Inputs', Inputs, envir = EnvRainValidation)
		}else{
			if(all(EnvRainValidation$Inputs == Inputs)){
				imois0 <- sort(EnvRainValidation$MONTH)
				imois1 <- sort(getMonthsInSeason(tclvalue(start_mois), tclvalue(end_mois), full = TRUE))
				iyear0 <- sort(EnvRainValidation$YEAR)
				iyear1 <- as.numeric(tclvalue(start_year)):as.numeric(tclvalue(end_year))
				if(identical(imois0, imois1) & identical(iyear0, iyear1)) do_extr <- 0
				else do_extr <- 1
			}else{
				do_extr <- 1
				assign('Inputs', Inputs, envir = EnvRainValidation)
				assign('valid.data', NULL, envir = EnvRainValidation)
			}
		}

		donne <- getCDTdata(file.stnfl, file.period)
		rfedata <- getNcdfOpenData(file.grdCDF)
		shpf <- getShpOpenData(file.plotShp)
		retValidParams <- list(clim.var = clim.var, stn = list(donne = donne, filestn = tclvalue(file.stnfl)),
						rfe = list(rfedata = rfedata, ncdir = tclvalue(dirNetCDF), ncformat = tclvalue(netCDFff)),
						dates = list(start_mois = tclvalue(start_mois), end_mois = tclvalue(end_mois),
									start_year = tclvalue(start_year), end_year = tclvalue(end_year)),
						shp = list(shpf = shpf, attr = tclvalue(Admin_var), id = tclvalue(namePoly)),
						rect = c(tclvalue(minlonRect), tclvalue(maxlonRect), tclvalue(minlatRect), tclvalue(maxlatRect)),
						select_type = tclvalue(select_type), dir2sav = tclvalue(file.save1), do_extr = do_extr)

		tkconfigure(main.win, cursor = 'watch')
		InsertMessagesTxt(main.txt.out, "Validation.................")
		tcl('update')

		tryCatch(outValiddata <<- ValidationDataFun(retValidParams),
		#warning = function(w) warningFun(w),
		error = function(e) errorFun(e), finally = {
			tkconfigure(main.win, cursor = '')
		})
		if(!is.null(outValiddata)) InsertMessagesTxt(main.txt.out, "Validation finished successfully")
	})

	#### Display stat table
	validStatTab <- NULL
	tkconfigure(stats.tab3, command = function(){
		if(!is.null(outValiddata)){
			if(tclvalue(dataType) == 'All Data'){
				dat2disp <- outValiddata$stat
				titleTab <- 'All-Data Statistics'
			}else{
				dat2disp <- outValiddata$sp.stat
				titleTab <- 'Spatial-Average Statistics'
			}
			retNBTab <- tableValidationNotebookTab_unik(tknotes, dat2disp, titleTab, validStatTab, AllOpenTabType, AllOpenTabData)
			validStatTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	#### Display scatter-plot
	notebookTab1 <- NULL
	tkconfigure(scatt.tab3, command = function(){
		if(!is.null(outValiddata)){
			imgContainer <- displayGGvsSatFun(tknotes, notebookTab1, outValiddata, dataType)
			if(!is.null(imgContainer)){
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab1, AllOpenTabType, AllOpenTabData)
				notebookTab1 <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		}
	})

	#### Display CDF
	notebookTab2 <- NULL
	tkconfigure(ecdf.tab3, command = function(){
		if(!is.null(outValiddata)){
			imgContainer <- displayCDFGGvsSatFun(tknotes, notebookTab2, outValiddata, dataType)
			if(!is.null(imgContainer)){
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab2, AllOpenTabType, AllOpenTabData)
				notebookTab2 <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		}
	})

	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}

